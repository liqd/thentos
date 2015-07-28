{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Thentos
    ( main
    , makeMain
    , createDefaultUser
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Concurrent (ThreadId, threadDelay, forkIO)
import Control.Exception (finally)
import Control.Monad (void, when, forever)
import Crypto.Random (ChaChaDRG, drgNew)
import Data.Acid (AcidState, openLocalStateFrom, createCheckpoint, closeAcidState)
import Data.Acid.Advanced (query', update')
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Either (isRight, isLeft)
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy(Proxy))
import System.Log.Logger (Priority(DEBUG, INFO, ERROR), removeAllHandlers)
import Text.Show.Pretty (ppShow)

import qualified Data.Map as Map

import System.Log.Missing (logger, announceAction)
import Thentos.Action
import Thentos.Action.Core (Action, ActionState(..), runActionWithPrivs, Ex)
import Thentos.Config
import Thentos.Frontend (runFrontend)
import Thentos.Smtp (checkSendmail)
import Thentos.Types
import Thentos.Util

import qualified Thentos.Backend.Api.Simple (runApi)
import qualified Thentos.Transaction as T


-- * main

main :: IO ()
main = makeMain emptyDB $ \ (actionState@(ActionState (st, _, _))) mBeConfig mFeConfig cmd ->
    case cmd of
        ShowDB -> do
            logger INFO "database contents:"
            query' st T.SnapShot >>= either (error "oops?") (logger INFO . ppShow)

        Run -> do
            let backend = maybe (return ())
                    (`Thentos.Backend.Api.Simple.runApi` actionState)
                    mBeConfig
            let frontend = maybe (return ())
                    (`runFrontend` actionState)
                    mFeConfig

            void $ concurrently backend frontend

        RunSso -> error "RunSso: not implemented."


-- * main with abstract commands

makeMain :: forall db . (db `Ex` DB) =>
    db -> (ActionState db -> Maybe HttpConfig -> Maybe HttpConfig -> Command -> IO ()) -> IO ()
makeMain initialDB commandSwitch =
  do
    config :: ThentosConfig <- getConfig "devel.config"
    checkSendmail (Tagged $ config >>. (Proxy :: Proxy '["smtp"]))

    st :: AcidState db
        <- announceAction "setting up acid-state" $ openLocalStateFrom ".acid-state/" initialDB
        -- (opening acid-state can take rather long if a large
        -- changelog needs to be replayed.  use asci-progress here?
        -- even though that would probably require patching
        -- acid-state.)

    rng :: MVar ChaChaDRG   <- drgNew >>= newMVar
    let actionState = ActionState (st, rng, config)
        log_path = config >>. (Proxy :: Proxy '["log", "path"])
        log_level = config >>. (Proxy :: Proxy '["log", "level"])
    configLogger log_path log_level
    _ <- createCheckpointLoop st 16000
    _ <- runGcLoop actionState $ config >>. (Proxy :: Proxy '["gc_interval"])
    createDefaultUser st (Tagged <$> config >>. (Proxy :: Proxy '["default_user"]))
    runActionWithPrivs [RoleAdmin] actionState $ autocreateMissingServices config

    let mBeConfig :: Maybe HttpConfig
        mBeConfig = Tagged <$> config >>. (Proxy :: Proxy '["backend"])

        mFeConfig :: Maybe HttpConfig
        mFeConfig = Tagged <$> config >>. (Proxy :: Proxy '["frontend"])

    logger INFO "Press ^C to abort."
    let run = do
            commandSwitch actionState mBeConfig mFeConfig $ config >>. (Proxy :: Proxy '["command"])
        finalize = do
            announceAction "creating checkpoint and shutting down acid-state" $
                createCheckpoint st >> closeAcidState st
            announceAction "shutting down hslogger" $
                removeAllHandlers

    run `finally` finalize


-- * helpers

-- | Garbage collect DB type.  (In this module because 'Thentos.Util' doesn't have 'Thentos.Action'
-- yet.  It takes the time interval in such a weird type so that it's easier to call with the
-- config.  This function should move and change in the future.)
runGcLoop :: (db `Ex` DB) => ActionState db -> Maybe Int -> IO ThreadId
runGcLoop _           Nothing         = forkIO $ return ()
runGcLoop actionState (Just interval) = forkIO . forever $ do
    runActionWithPrivs [RoleAdmin] actionState collectGarbage
    threadDelay $ interval * 1000 * 1000


-- | If default user is 'Nothing' or user with 'UserId 0' exists, do
-- nothing.  Otherwise, create default user.
createDefaultUser :: (db `Ex` DB) => AcidState db -> Maybe DefaultUserConfig -> IO ()
createDefaultUser _ Nothing = return ()
createDefaultUser st (Just (getDefaultUser -> (userData, roles))) = do
    eq <- query' st $ T.LookupUser (UserId 0)
    when (isLeft eq) $ do
        -- user
        user <- makeUserFromFormData userData
        logger DEBUG $ "No users.  Creating default user: " ++ ppShow (UserId 0, user)
        eu <- update' st $ T.AddUser user

        if eu == Right (UserId 0)
            then logger DEBUG $ "[ok]"
            else logger ERROR $ "failed to create default user: " ++ ppShow (UserId 0, eu, user)

        -- roles
        logger DEBUG $ "Adding default user to roles: " ++ ppShow roles
        result <- mapM (update' st . T.AssignRole (UserA . UserId $ 0)) roles

        if all isRight result
            then logger DEBUG $ "[ok]"
            else logger ERROR $ "failed to assign default user to roles: " ++ ppShow (UserId 0, result, user, roles)

-- | Autocreate any services that are listed in the config but don't exist in the DB.
-- Dies with an error if the default "proxy" service ID is repeated in the "proxies" section.
autocreateMissingServices :: (db `Ex` DB) => ThentosConfig -> Action db ()
autocreateMissingServices cfg = do
    dieOnDuplicates
    mapM_ (autocreateServiceIfMissing'P agent) allSids
  where
    dieOnDuplicates  = case mDefaultProxySid of
        Just sid -> when (sid `elem` proxySids) . error $ show sid ++ " mentioned twice in config"
        Nothing  -> return ()
    allSids          = maybeToList mDefaultProxySid ++ proxySids
    mDefaultProxySid = ServiceId <$> cfg >>. (Proxy :: Proxy '["proxy", "service_id"])
    proxySids        = Map.keys $ getProxyConfigMap cfg
    agent            = UserA $ UserId 0

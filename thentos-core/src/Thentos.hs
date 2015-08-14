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
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Either (isRight, isLeft)
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy(Proxy))
import Database.PostgreSQL.Simple (Connection)
import System.Log.Logger (Priority(DEBUG, INFO, ERROR), removeAllHandlers)
import Text.Show.Pretty (ppShow)

import qualified Data.Map as Map

import System.Log.Missing (logger, announceAction)
import Thentos.Action
import Thentos.Action.Core (Action, ActionState(..), runActionWithPrivs)
import Thentos.Config
import Thentos.Frontend (runFrontend)
import Thentos.Smtp (checkSendmail)
import Thentos.Types
import Thentos.Util

import qualified Thentos.Backend.Api.Simple (runApi)
import qualified Thentos.Transaction as T


-- * main

main :: IO ()
main = makeMain $ \ (actionState@(ActionState (st, _, _))) mBeConfig mFeConfig cmd ->
    case cmd of
        Run -> do
            let backend = maybe (return ())
                    (`Thentos.Backend.Api.Simple.runApi` actionState)
                    mBeConfig
            let frontend = maybe (return ())
                    (`runFrontend` actionState)
                    mFeConfig

            void $ concurrently backend frontend


-- * main with abstract commands

makeMain ::
    (ActionState -> Maybe HttpConfig -> Maybe HttpConfig -> Command -> IO ()) -> IO ()
makeMain commandSwitch =
  do
    config :: ThentosConfig <- getConfig "devel.config"
    checkSendmail (Tagged $ config >>. (Proxy :: Proxy '["smtp"]))

    rng :: MVar ChaChaDRG   <- drgNew >>= newMVar
    let actionState = ActionState (error "./src/Thentos.hs:80", rng, config)
        log_path = config >>. (Proxy :: Proxy '["log", "path"])
        log_level = config >>. (Proxy :: Proxy '["log", "level"])
    configLogger log_path log_level
    _ <- runGcLoop actionState $ config >>. (Proxy :: Proxy '["gc_interval"])
    runActionWithPrivs [RoleAdmin] actionState $ autocreateMissingServices config

    let mBeConfig :: Maybe HttpConfig
        mBeConfig = Tagged <$> config >>. (Proxy :: Proxy '["backend"])

        mFeConfig :: Maybe HttpConfig
        mFeConfig = Tagged <$> config >>. (Proxy :: Proxy '["frontend"])

    logger INFO "Press ^C to abort."
    let run = do
            commandSwitch actionState mBeConfig mFeConfig $ config >>. (Proxy :: Proxy '["command"])
        finalize = do
            announceAction "shutting down hslogger" $
                removeAllHandlers

    run `finally` finalize


-- * helpers

-- | Garbage collect DB type.  (In this module because 'Thentos.Util' doesn't have 'Thentos.Action'
-- yet.  It takes the time interval in such a weird type so that it's easier to call with the
-- config.  This function should move and change in the future.)
runGcLoop :: ActionState -> Maybe Int -> IO ThreadId
runGcLoop _           Nothing         = forkIO $ return ()
runGcLoop actionState (Just interval) = forkIO . forever $ do
    runActionWithPrivs [RoleAdmin] actionState collectGarbage
    threadDelay $ interval * 1000 * 1000


-- | If default user is 'Nothing' or user with 'UserId 0' exists, do
-- nothing.  Otherwise, create default user.
createDefaultUser :: Connection -> Maybe DefaultUserConfig -> IO ()
createDefaultUser = error "./src/Thentos.hs:118"

-- | Autocreate any services that are listed in the config but don't exist in the DB.
-- Dies with an error if the default "proxy" service ID is repeated in the "proxies" section.
autocreateMissingServices :: ThentosConfig -> Action ()
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

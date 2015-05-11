{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Thentos (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (bracket_, finally)
import Control.Monad (void)
import Control.Monad (when)
import "crypto-random" Crypto.Random (SystemRNG, createEntropyPool, cprgCreate)
import Data.Acid (AcidState, openLocalStateFrom, createCheckpoint, closeAcidState)
import Data.Acid.Advanced (query', update')
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Either (isRight, isLeft)
import Data.Proxy (Proxy(Proxy))
import System.Log.Logger (Priority(INFO), removeAllHandlers)
import System.Log (Priority(DEBUG, ERROR))
import Text.Show.Pretty (ppShow)

import System.Log.Missing (logger)
import Thentos.Action
import Thentos.Action.Core
import Thentos.Config
import Thentos.Frontend (runFrontend)
import Thentos.Types
import Thentos.Util

import qualified Thentos.Backend.Api.Adhocracy3 (runBackend)
import qualified Thentos.Backend.Api.Simple (runBackend)
import qualified Thentos.Transaction as T


-- * main

main :: IO ()
main =
  do
    let notify :: String -> IO a -> IO a
        notify msg action = bracket_ (logger INFO msg)
                                     (logger INFO $ msg ++ ": [ok]") action

    st :: AcidState DB <- notify "setting up acid-state" $ openLocalStateFrom ".acid-state/" emptyDB
        -- (opening acid-state can take rather long if a large
        -- changelog needs to be replayed.  use asci-progress here?
        -- even though that would probably require patching
        -- acid-state.)

    rng :: MVar SystemRNG <- createEntropyPool >>= newMVar . cprgCreate
    config :: ThentosConfig <- getConfig "devel.config"

    let actionState = ActionState (st, rng, config)

    configLogger
    _ <- createCheckpointLoop st 16000
    _ <- runGcLoop $ config >>. (Proxy :: Proxy '["gc_interval"])
    createDefaultUser st (Tagged <$> config >>. (Proxy :: Proxy '["default_user"]))

    let mBeConfig :: Maybe HttpConfig
        mBeConfig = Tagged <$> config >>. (Proxy :: Proxy '["backend"])

        mFeConfig :: Maybe HttpConfig
        mFeConfig = Tagged <$> config >>. (Proxy :: Proxy '["frontend"])

    logger INFO "Press ^C to abort."
    let run = case config >>. (Proxy :: Proxy '["command"]) of
            ShowDB -> do
                logger INFO "database contents:"
                query' st T.SnapShot >>= either (error "oops?") (logger INFO . ppShow)

            Run -> do
                let backend = maybe (return ())
                        (`Thentos.Backend.Api.Simple.runBackend` actionState)
                        mBeConfig
                let frontend = maybe (return ())
                        (`runFrontend` actionState)
                        mFeConfig

                void $ concurrently backend frontend

            RunA3 -> do
                maybe (error "command `runa3` requires `--runbackend`")
                    (`Thentos.Backend.Api.Adhocracy3.runBackend` actionState)
                    mBeConfig

    let finalize = do
            notify "creating checkpoint and shutting down acid-state" $
                createCheckpoint st >> closeAcidState st
            notify "shutting down hslogger" $
                removeAllHandlers

    run `finally` finalize


-- * garbage collection
runGcLoop :: AcidState st -> Maybe Timeout -> IO ThreadId
runGcLoop _         Nothing         = forkIO $ return ()
runGcLoop acidState (Just interval) = forkIO . forever $ do
    threadDelay $ fromTimeout interval * 1000
    update' st T.collectGarbage


-- | If default user is 'Nothing' or user with 'UserId 0' exists, do
-- nothing.  Otherwise, create default user.
createDefaultUser :: AcidState DB -> Maybe DefaultUserConfig -> IO ()
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
        result <- mapM (\ role -> update' st (T.AssignRole (UserA (UserId 0)) role)) roles

        if all isRight result
            then logger DEBUG $ "[ok]"
            else logger ERROR $ "failed to assign default user to roles: " ++ ppShow (UserId 0, result, user, roles)

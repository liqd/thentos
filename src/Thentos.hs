{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}

{-# OPTIONS  #-}

module Thentos (main) where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (bracket_, finally)
import Control.Monad (void)
import Crypto.Random (SystemRNG, createEntropyPool, cprgCreate)
import Data.Acid (AcidState, openLocalStateFrom, createCheckpoint, closeAcidState)
import Data.Acid.Advanced (query')
import Data.String.Conversions ((<>))
import System.Log.Logger (Priority(INFO), removeAllHandlers)
import Text.Show.Pretty (ppShow)

import Thentos.Config (configLogger, getCommand, Command(..), ThentosConfig(..), BackendConfig(BackendConfig), FrontendConfig(FrontendConfig))
import Thentos.Types
import Thentos.DB
import Thentos.Frontend (runFrontend)
import System.Log.Missing (logger)

import qualified Thentos.Backend.Api.Simple (runBackend)
import qualified Thentos.Backend.Api.Adhocracy3 (runBackend)


-- * main

main :: IO ()
main =
  do
    let notify :: String -> IO a -> IO a
        notify msg action = bracket_ (logger INFO msg)
                                     (logger INFO $ msg ++ ": [ok]") action

    st :: AcidState DB <- notify "setting up acid-state" $ openLocalStateFrom ".acid-state/" emptyDB
    rng :: MVar SystemRNG <- createEntropyPool >>= newMVar . cprgCreate

    configLogger

    eCmd <- getCommand "devel.config"
    -- logger DEBUG (ppShow eCmd)
    let run = case eCmd of
            Right ShowDB -> do
                logger INFO "database contents:"
                query' st (SnapShot allowEverything) >>= either (error "oops?") (logger INFO . ppShow)

            Right (Run config) -> do
                createDefaultUser st (defaultUser config)

                let backend = case backendConfig config of
                        Nothing -> return ()
                        Just (BackendConfig backendPort) -> do
                            logger INFO $ "running rest api on localhost:" <> show backendPort <> "."
                            Thentos.Backend.Api.Simple.runBackend backendPort (st, rng, config)

                let frontend = case frontendConfig config of
                        Nothing -> return ()
                        Just (FrontendConfig frontendPort) -> do
                            logger INFO $ "running frontend on localhost:" <> show frontendPort <> "."
                            runFrontend "localhost" frontendPort (st, rng, config)

                _ <- createCheckpointLoop st 16000 Nothing
                logger INFO "Press ^C to abort."
                void $ concurrently backend frontend

            Right (RunA3 config) -> do
                createDefaultUser st (defaultUser config)
                _ <- createCheckpointLoop st 16000 Nothing

                case backendConfig config of
                        Nothing -> error "command `runa3` requires `--runbackend`"
                        Just (BackendConfig backendPort) -> do
                            logger INFO $ "running a3 rest api on localhost:" <> show backendPort <> "."
                            logger INFO "Press ^C to abort."
                            Thentos.Backend.Api.Adhocracy3.runBackend backendPort (st, rng, config)

            Left e -> error $ "error parsing config (shell env, cli, or config files): " ++ show e

    let finalize = do
            notify "creating checkpoint and shutting down acid-state" $
                createCheckpoint st >> closeAcidState st
            notify "shutting down hslogger" $
                removeAllHandlers

    run `finally` finalize

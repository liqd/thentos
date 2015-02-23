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
import Control.Exception (bracket, finally)
import Control.Monad (void)
import Crypto.Random (SystemRNG, createEntropyPool, cprgCreate)
import Data.Acid (AcidState, openLocalStateFrom, createCheckpoint, closeAcidState)
import Data.Acid.Advanced (query')
import Data.String.Conversions ((<>))
import System.IO (hPutStrLn, stderr)
import System.Log.Logger (Priority(DEBUG, INFO), removeAllHandlers)
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
        notify msg action = bracket (logger INFO msg) (\ _ -> logger INFO $ msg ++ ": [ok]") (\ _ -> action)

    st :: AcidState DB <- notify "setting up acid-state" $ openLocalStateFrom ".acid-state/" emptyDB
    rng :: MVar SystemRNG <- createEntropyPool >>= newMVar . cprgCreate

    configLogger

    Right cmd <- getCommand "devel.config"
    logger DEBUG (ppShow cmd)
    let run = case cmd of
            ShowDB -> do
                putStrLn "database contents:"
                query' st (SnapShot allowEverything) >>= either (error "oops?") (putStrLn . ppShow)

            Run config -> do
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
                hPutStrLn stderr "Press ^C to abort."
                void $ concurrently backend frontend

            RunA3 config -> do
                createDefaultUser st (defaultUser config)
                _ <- createCheckpointLoop st 16000 Nothing

                case backendConfig config of
                        Nothing -> error "command `runa3` requires `--runbackend`"
                        Just (BackendConfig backendPort) -> do
                            logger INFO $ "running a3 rest api on localhost:" <> show backendPort <> "."
                            hPutStrLn stderr "Press ^C to abort."
                            Thentos.Backend.Api.Adhocracy3.runBackend backendPort (st, rng, config)

    let finalize = do
            notify "creating checkpoint and shutting down acid-state" $
                createCheckpoint st >> closeAcidState st
            notify "shutting down hslogger" $
                removeAllHandlers

    run `finally` finalize

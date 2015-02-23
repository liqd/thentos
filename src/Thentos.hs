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
import Control.Exception (finally)
import Control.Monad (void)
import Crypto.Random (SystemRNG, createEntropyPool, cprgCreate)
import Data.Acid (AcidState, openLocalStateFrom, createCheckpoint, closeAcidState)
import Data.Acid.Advanced (query')
import Data.String.Conversions ((<>))
import System.Log.Logger (Priority(DEBUG), removeAllHandlers)
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
    putStr "setting up acid-state..."
    st :: AcidState DB <- openLocalStateFrom ".acid-state/" emptyDB
    putStrLn " [ok]"

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
                            putStrLn $ "running rest api on localhost:" <> show backendPort <> "."
                            Thentos.Backend.Api.Simple.runBackend backendPort (st, rng, config)

                let frontend = case frontendConfig config of
                        Nothing -> return ()
                        Just (FrontendConfig frontendPort) -> do
                            putStrLn $ "running frontend on localhost:" <> show frontendPort <> "."
                            runFrontend "localhost" frontendPort (st, rng, config)

                _ <- createCheckpointLoop st 16000 Nothing
                putStrLn "Press ^C to abort."
                void $ concurrently backend frontend

            RunA3 config -> do
                createDefaultUser st (defaultUser config)
                _ <- createCheckpointLoop st 16000 Nothing

                case backendConfig config of
                        Nothing -> error "command `runa3` requires `--runbackend`"
                        Just (BackendConfig backendPort) -> do
                            putStrLn $ "running a3 rest api on localhost:" <> show backendPort <> "."
                            putStrLn "Press ^C to abort."
                            Thentos.Backend.Api.Adhocracy3.runBackend backendPort (st, rng, config)

    let finalize = do
            putStr "creating checkpoint and shutting down acid-state..."
            createCheckpoint st
            closeAcidState st
            putStrLn " [ok]"

            putStr "shutting down hslogger..."
            removeAllHandlers
            putStrLn " [ok]"

    run `finally` finalize

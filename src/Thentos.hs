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
import Control.Exception (SomeException, throw, catch)
import Control.Monad (void)
import Crypto.Random (SystemRNG, createEntropyPool, cprgCreate)
import Data.Acid (AcidState, openLocalStateFrom, createCheckpoint, closeAcidState)
import Data.Acid.Advanced (query')
import Data.String.Conversions ((<>))
import System.Log.Logger (Priority(DEBUG), removeAllHandlers)
import Text.Show.Pretty (ppShow)

import Config (configLogger, getCommand, Command(..), ThentosConfig(..), BackendConfig(BackendConfig), FrontendConfig(FrontendConfig))
import Types
import DB
import Backend.Api.Simple (runBackend, apiDocs)
import Frontend (runFrontend)
import System.Log.Missing (logger)


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
                            runBackend backendPort (st, rng, config)

                let frontend = case frontendConfig config of
                        Nothing -> return ()
                        Just (FrontendConfig frontendPort) -> do
                            putStrLn $ "running frontend on localhost:" <> show frontendPort <> "."
                            runFrontend "localhost" frontendPort (st, rng, config)

                _ <- createCheckpointLoop st 16000 Nothing
                putStrLn "Press ^C to abort."
                void $ concurrently backend frontend

            Docs -> putStrLn apiDocs

    let finalize = do
            putStr "creating checkpoint and shutting down acid-state..."
            createCheckpoint st
            closeAcidState st
            putStrLn " [ok]"

            putStr "shutting down hslogger..."
            removeAllHandlers
            putStrLn " [ok]"

    catch run (\ (e :: SomeException) -> finalize >> throw e)
    finalize

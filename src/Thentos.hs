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
import System.Log.Logger (removeAllHandlers)
import Text.Show.Pretty (ppShow)

import Config (configLogger, getCommand, Command(..), ThentosConfig(..), BackendConfig(BackendConfig), FrontendConfig(FrontendConfig))
import Types
import DB
import Backend.Api.Simple (runApi, apiDocs)
import Frontend (runFrontend)


-- * main

main :: IO ()
main =
  do
    putStr "setting up acid-state..."
    st :: AcidState DB <- openLocalStateFrom ".acid-state/" emptyDB
    putStrLn " [ok]"

    rng :: MVar SystemRNG <- createEntropyPool >>= newMVar . cprgCreate

    createGod st True  -- FIXME: remove this from production code
    configLogger

    Right cmd <- getCommand
    let run = case cmd of
            ShowDB -> do
                putStrLn "database contents:"
                query' st (SnapShot allowEverything) >>= either (error "oops?") (putStrLn . ppShow)
            Run config -> do
                let backend = case backendConfig config of
                        Nothing -> return ()
                        Just (BackendConfig backendPort) -> do
                            putStrLn $ "running rest api on localhost:" <> show backendPort <> "."
                            runApi backendPort (st, rng)

                let frontend = case frontendConfig config of
                        Nothing -> return ()
                        Just (FrontendConfig frontendPort) -> do
                            putStrLn $ "running frontend on localhost:" <> show frontendPort <> "."
                            putStrLn "Press ^C to abort."
                            runFrontend "localhost" frontendPort (st, rng)
                _ <- createCheckpointLoop st 16000 Nothing
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

-- curl -H "Content-Type: application/json" -X PUT -d '{"userGroups":[],"userPassword":"dummy","userName":"dummy","userID":3,"userEmail":"dummy"}' -v http://localhost:8001/v0.0.1/user/id/3
-- curl -H "Content-Type: application/json" -X POST -d '{"userGroups":[],"userPassword":"dummy","userName":"dummy","userEmail":"dummy"}' -v http://localhost:8001/v0.0.1/user

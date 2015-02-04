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
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS  #-}

module Thentos (main) where

import Control.Applicative ((<$>))
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (SomeException, throw, catch)
import Control.Monad (void)
import Crypto.Random (SystemRNG, createEntropyPool, cprgCreate)
import Data.Acid (AcidState, openLocalStateFrom, createCheckpoint, closeAcidState)
import Data.Acid.Advanced (query', update')
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs, (<>))
import Safe (readMay)
import System.Environment (getArgs)
import System.IO (stderr)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler.Simple (formatter, fileHandler, streamHandler)
import System.Log.Logger (removeAllHandlers, Priority(DEBUG), updateGlobalLogger, setLevel, setHandlers)
import System.Log.Missing (loggerName)
import Text.Show.Pretty (ppShow)

import qualified Data.Aeson as Aeson

import Types
import DB
import Backend.Api.Simple (runApi, apiDocs)
import Frontend (runFrontend)


-- * main

main :: IO ()
main =
  do
    args <- getArgs
    putStr "setting up acid-state..."
    st :: AcidState DB <- openLocalStateFrom ".acid-state/" emptyDB
    putStrLn " [ok]"

    rng :: MVar SystemRNG <- createEntropyPool >>= newMVar . cprgCreate

    createGod st True
    configLogger

    let switch ["-s"] = do
            putStrLn "database contents:"
            query' st (SnapShot allowEverything) >>= either (error "oops?") (putStrLn . ppShow)
        switch ["-a"] = do
            putStrLn "adding user from stdin to database:"
            Just (user :: User) <- Aeson.decode . cs <$> getContents
            void . update' st $ AddUser user allowEverything
        switch ["-a2"] = do
            putStrLn "adding dummy user to database:"
            void . update' st $ AddUser (User "dummy" "dummy" "dummy" [] []) allowEverything
        switch ["-r"] = switch ["-r", "", ""]
        switch ["-r", a] = switch ["-r", a, ""]
        switch ["-r"
               , fromMaybe 8001 . readMay -> backendPort
               , fromMaybe 8002 . readMay -> frontendPort
               ] = do
            putStrLn $ "running rest api on localhost:" <> show backendPort <> "."
            putStrLn $ "running frontend on localhost:" <> show frontendPort <> "."
            putStrLn "Press ^C to abort."
            _ <- createCheckpointLoop st 16000 Nothing
            void $ concurrently
                (runFrontend "localhost" frontendPort (st, rng))
                (runApi backendPort (st, rng))
        switch ["--docs"] = putStrLn apiDocs
        switch _ = error $ "bad arguments: " <> show args

        finalize = do
          putStr "creating checkpoint and shutting down acid-state..."
          createCheckpoint st
          closeAcidState st
          putStrLn " [ok]"

          putStr "shutting down hslogger..."
          removeAllHandlers
          putStrLn " [ok]"

    catch (switch args) (\ (e :: SomeException) -> finalize >> throw e)
    finalize

-- curl -H "Content-Type: application/json" -X PUT -d '{"userGroups":[],"userPassword":"dummy","userName":"dummy","userID":3,"userEmail":"dummy"}' -v http://localhost:8001/v0.0.1/user/id/3
-- curl -H "Content-Type: application/json" -X POST -d '{"userGroups":[],"userPassword":"dummy","userName":"dummy","userEmail":"dummy"}' -v http://localhost:8001/v0.0.1/user



configLogger :: IO ()
configLogger = do
    let fmt = simpleLogFormatter "$utcTime *$prio* [$pid][$tid] -- $msg"
    fHandler <- (\ h -> h { formatter = fmt }) <$> fileHandler "./log/thentos.log" DEBUG
    sHandler <- (\ h -> h { formatter = fmt }) <$> streamHandler stderr DEBUG

    updateGlobalLogger loggerName $
        System.Log.Logger.setLevel DEBUG .
        setHandlers [sHandler, fHandler]

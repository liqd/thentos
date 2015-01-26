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
import Control.Exception (SomeException, throw, catch)
import Control.Monad (void, when)
import Data.Acid (AcidState, openLocalStateFrom, createCheckpoint, closeAcidState)
import Data.Acid.Advanced (query', update')
import Data.Either (isLeft)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs, (<>))
import Safe (readMay)
import System.Environment (getArgs)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson

import Types
import DB
import Api (runApi, apiDocs)
import Frontend (runFrontend)


-- * main

main :: IO ()
main =
  do
    args <- getArgs
    putStr "setting up acid-state..."
    st :: AcidState DB <- openLocalStateFrom ".acid-state/" emptyDB
    putStrLn " [ok]"

    createGod st True

    let switch ["-s"] = do
            putStrLn "database contents:"
            putStrLn "Users:"
            query' st (AllUserIDs thentosPublic)       >>= either (error "oops?") (mapM_ (putStrLn . cs . Aeson.encodePretty))
            putStrLn "Services:"
            query' st (AllServiceIDs thentosPublic)    >>= either (error "oops?") (mapM_ (putStrLn . cs . Aeson.encodePretty))
            putStrLn "Sessions:"
            query' st (AllSessionTokens thentosPublic) >>= either (error "oops?") (mapM_ (putStrLn . cs . Aeson.encodePretty))
        switch ["-a"] = do
            putStrLn "adding user from stdin to database:"
            Just (user :: User) <- Aeson.decode . cs <$> getContents
            void $ update' st $ AddUser user thentosPublic
        switch ["-a2"] = do
            putStrLn "adding dummy user to database:"
            void . update' st $ AddUser (User "dummy" "dummy" "dummy" [] []) thentosPublic
        switch ["-a3"] = do
            putStrLn "adding dummy service to database:"
            sid <- update' st $ AddService thentosPublic
            putStrLn $ "Service id: " ++ show sid
        switch ["-r"] = switch ["-r", "", ""]
        switch ["-r", a] = switch ["-r", a, ""]
        switch ["-r"
               , fromMaybe 8001 . readMay -> backendPort
               , fromMaybe 8002 . readMay -> frontendPort
               ] = do
            putStrLn $ "running rest api on localhost:" <> show backendPort <> "."
            putStrLn $ "running frontend on localhost:" <> show frontendPort <> "."
            putStrLn $ "Press ^C to abort."
            _ <- createCheckpointLoop st 16000 Nothing
            void $ concurrently
                (runFrontend frontendPort st)
                (runApi backendPort st)
        switch ["--docs"] = putStrLn apiDocs
        switch _ = error $ "bad arguments: " <> show args

        finalize = do
          putStr "creating checkpoint and shutting down acid-state..."
          createCheckpoint st
          closeAcidState st
          putStrLn " [ok]"

    catch (switch args) (\ (e :: SomeException) -> finalize >> throw e)
    finalize

-- curl -H "Content-Type: application/json" -X PUT -d '{"userGroups":[],"userPassword":"dummy","userName":"dummy","userID":3,"userEmail":"dummy"}' -v http://localhost:8001/v0.0.1/user/id/3
-- curl -H "Content-Type: application/json" -X POST -d '{"userGroups":[],"userPassword":"dummy","userName":"dummy","userEmail":"dummy"}' -v http://localhost:8001/v0.0.1/user

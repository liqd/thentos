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

module Thentos
where

import Control.Applicative ((<$>))
import Control.Exception (SomeException, throw, catch)
import Control.Monad (void)
import Data.Acid (AcidState, openLocalStateFrom, createCheckpoint, closeAcidState)
import Data.Acid.Advanced (query', update')
import Data.Data (Proxy(Proxy))
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs, (<>))
import LIO (evalLIO)
import LIO.DCLabel (dcPublic)
import Network.Wai.Handler.Warp (run)
import Safe (readMay)
import Servant.Server (serve)
import System.Environment (getArgs)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson

import Servant.Docs (docs, markdown)

import Types
import DB
import Api


-- * main

main :: IO ()
main =
  do
    args <- getArgs
    putStr "setting up acid-state..."
    st :: AcidState DB <- openLocalStateFrom ".acid-state/" emptyDB
    putStrLn " [ok]"

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
        switch ["-r"] = switch ["-r", ""]
        switch ["-r", fromMaybe 8001 . readMay -> port] = do
            putStrLn $ "running rest api on localhost:" <> show port <> ".  press ^C to abort."
            createCheckpointLoop st 16000 Nothing
            run port $ serve (Proxy :: Proxy App) (app st)
        switch ["--docs"] = do
            let api = docs (Proxy :: Proxy App)
            putStrLn $ markdown api
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

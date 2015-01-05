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

{-# OPTIONS -fwarn-unused-imports -fwarn-incomplete-patterns #-}

module TestMain
where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Monad (void)
import Data.Acid (AcidState, openLocalStateFrom, closeAcidState, query, update)
import Data.Data (Proxy(Proxy))
import Data.Functor.Infix ((<$>))
import Data.Maybe (fromMaybe)
import Data.String.Conversions (SBS, LBS, cs)
import Data.Thyme (getCurrentTime)
import Filesystem (removeTree)
import GHC.Exts (fromString)
import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)
import Test.Hspec (hspec, describe, it, before, after, shouldBe, shouldThrow,
    anyException, shouldSatisfy)

import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types.Status as C
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LBS

import Api
import DB
import Types


data Config =
    Config
      { dbPath :: FilePath
      , restPort :: Int
      }
  deriving (Eq, Show)

config :: Config
config =
    Config
      { dbPath = ".test-db/"
      , restPort = 8002
      }


main :: IO ()
main = hspec $ do
  describe "DB" . before setupDB . after teardownDB $ do
    describe "AddUser, LookupUser, DeleteUser" $ do
      it "works" . withDB $ \ st -> do
        uid <- update st $ AddUser user1
        Just user1' <- query st $ LookupUser uid
        user1' `shouldBe` user1
        update st $ DeleteUser (UserId 1)
        u <- query st $ LookupUser (UserId 1)
        u `shouldBe` Nothing

      it "hspec meta: `setupDB, teardownDB` are called once for every `it` here." . withDB $ \ st -> do
        uids <- query st AllUserIDs
        uids `shouldBe` [UserId 0, UserId 1]

    describe "AddService, LookupService, DeleteService" $ do
      it "works" . withDB $ \st -> do
        service1_id <- update st AddService
        service2_id <- update st AddService
        Just service1 <- query st $ LookupService service1_id
        Just service2 <- query st $ LookupService service2_id
        service1 `shouldBe` service1 -- sanity check for reflexivity of Eq
        service1 `shouldSatisfy` (/= service2) -- should have different keys
        update st $ DeleteService service1_id
        Nothing <- query st $ LookupService service1_id
        return ()

    describe "StartSession" $ do
      it "works" . withDB $ \ st -> do
        from <- TimeStamp <$> getCurrentTime
        to <- TimeStamp <$> getCurrentTime
        update st (StartSession (UserId 0) "nosuchservice" from to) `shouldThrow` anyException
        sid :: ServiceId <- update st AddService
        void $ update st (StartSession (UserId 0) sid from to)

  describe "Api" . before setupApi . after teardownApi $ do
    describe "/user/" $ do
      it "works" $ \ _ -> do
        C.withManager C.defaultManagerSettings $ \ manager -> do
          req :: C.Request <- mkreq "POST" "/user" Nothing (Right $ User "1" "2" "3" [] Nothing)
          res :: C.Response LBS <- C.httpLbs req manager
          C.statusCode (C.responseStatus res) `shouldBe` 201


user1, user2, user3 :: User
user1 = User "name1" "passwd" "em@il" [] Nothing
user2 = User "name2" "passwd" "em@il" ["group1", "group2"] Nothing
user3 = User "name3" "3" "3" ["23"] Nothing

withDB :: (AcidState DB -> IO ()) -> IO ()
withDB prog = do
  st <- openLocalStateFrom (dbPath config) emptyDB
  prog st
  closeAcidState st

setupDB :: IO ()
setupDB = withDB $ \ st -> do
    update_ st $ AddUser user1
    update_ st $ AddUser user2

teardownDB :: () -> IO ()
teardownDB _ = removeTree $ fromString (dbPath config)

setupApi :: IO (Async ())
setupApi = async . withDB $ \ st -> do
    run (restPort config) $ serve (Proxy :: Proxy App) (app st)

teardownApi :: Async () -> IO ()
teardownApi = cancel

mkreq :: Aeson.ToJSON a => SBS -> String -> Maybe SBS -> Either LBS a -> IO C.Request
mkreq method path queryString (either id Aeson.encodePretty -> body) = do
    req <- C.parseUrl $ "http://localhost:" ++ show (restPort config) ++ path  -- FIXME: is there a `parseUrl` that eliminates double "/"?

    let defaultHeaders =
          ("Content-Type", "application/json") :
          ("Content-length", cs . show . LBS.length $ body) :
          []

    return $ req { C.method         = method
                 , C.checkStatus    = \ _ _ _ -> Nothing
                 , C.requestBody    = C.RequestBodyLBS body
                 , C.queryString    = fromMaybe "" queryString
                 , C.requestHeaders = defaultHeaders
                 }

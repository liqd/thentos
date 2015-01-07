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

module TestMain
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Data.Acid (AcidState, openLocalStateFrom, closeAcidState)
import Data.Data (Proxy(Proxy))
import Data.Functor.Infix ((<$>))
import Data.Maybe (fromMaybe)
import Data.String.Conversions (SBS, LBS, cs)
import Data.Thyme (getCurrentTime)
import Filesystem (removeTree)
import GHC.Exts (fromString)
import LIO.DCLabel (DCLabel, (%%))
import LIO (LIOState(LIOState), evalLIO)
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
        uid <- evalLIO (updateLIO st $ AddUser user1) allowAll
        Just user1' <- evalLIO (queryLIO st $ LookupUser uid) allowAll
        user1' `shouldBe` user1
        evalLIO (updateLIO st $ DeleteUser (UserId 1)) allowAll
        u <- evalLIO (queryLIO st $ LookupUser (UserId 1)) allowAll
        u `shouldBe` Nothing

      it "hspec meta: `setupDB, teardownDB` are called once for every `it` here." . withDB $ \ st -> do
        uids <- evalLIO (queryLIO st AllUserIDs) allowAll
        uids `shouldBe` [UserId 0, UserId 1]

    describe "AddService, LookupService, DeleteService" $ do
      it "works" . withDB $ \st -> do
        service1_id <- evalLIO (updateLIO st AddService) allowAll
        service2_id <- evalLIO (updateLIO st AddService) allowAll
        Just service1 <- evalLIO (queryLIO st $ LookupService service1_id) allowAll
        Just service2 <- evalLIO (queryLIO st $ LookupService service2_id) allowAll
        service1 `shouldBe` service1 -- sanity check for reflexivity of Eq
        service1 `shouldSatisfy` (/= service2) -- should have different keys
        evalLIO (updateLIO st $ DeleteService service1_id) allowAll
        Nothing <- evalLIO (queryLIO st $ LookupService service1_id) allowAll
        return ()

    describe "StartSession" $ do
      it "works" . withDB $ \ st -> do
        from <- TimeStamp <$> getCurrentTime
        to <- TimeStamp <$> getCurrentTime
        evalLIO (updateLIO st (StartSession (UserId 0) "nosuchservice" from to)) allowAll `shouldThrow` anyException
        sid :: ServiceId <- evalLIO (updateLIO st AddService) allowAll
        evalLIO (updateLIO_ st (StartSession (UserId 0) sid from to)) allowAll

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
    evalLIO (updateLIO_ st $ AddUser user1) allowAll
    evalLIO (updateLIO_ st $ AddUser user2) allowAll

allowAll :: LIOState DCLabel
allowAll = LIOState (True %% True) (True %% True)

teardownDB :: () -> IO ()
teardownDB _ = removeTree $ fromString (dbPath config)

setupApi :: IO (Async ())
setupApi = async . withDB $ \ st -> do
    run (restPort config) $ serve (Proxy :: Proxy App) (app st)
    threadDelay $ 100 * 1000

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

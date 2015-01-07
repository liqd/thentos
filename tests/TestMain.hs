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

import Control.Monad.State (liftIO)
import Data.Acid (AcidState, openLocalStateFrom, closeAcidState)
import Data.Data (Proxy(Proxy))
import Data.Functor.Infix ((<$>))
import Data.String.Conversions (LBS, SBS, cs)
import Data.Thyme (getCurrentTime)
import Filesystem (removeTree)
import GHC.Exts (fromString)
import LIO.DCLabel (DCLabel, (%%))
import LIO (LIOState(LIOState), evalLIO)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Method (Method)
import Network.Wai (Application, StreamingBody, requestMethod, requestBody, pathInfo, requestHeaders)
import Network.Wai.Internal (Response(ResponseFile, ResponseBuilder, ResponseStream, ResponseRaw))
import Network.Wai.Test (Session, SRequest(SRequest),
    runSession, request, srequest, setPath, defaultRequest, simpleStatus, simpleBody)
import Servant.Server (serve)
import Test.Hspec (hspec, describe, it, before, after, shouldBe, shouldThrow,
    anyException, shouldSatisfy)
import Text.Show.Pretty (ppShow)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Types.Status as C

import Api
import DB
import Types


-- * config

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


-- * test suite

main :: IO ()
main = hspec $ do
  describe "DB" . before setupDB . after teardownDB $ do
    describe "AddUser, LookupUser, DeleteUser" $ do
      it "works" $ \ st -> do
        uid <- evalLIO (updateLIO st $ AddUser user1) allowAll
        Just user1' <- evalLIO (queryLIO st $ LookupUser uid) allowAll
        user1' `shouldBe` user1
        evalLIO (updateLIO st $ DeleteUser (UserId 1)) allowAll
        u <- evalLIO (queryLIO st $ LookupUser (UserId 1)) allowAll
        u `shouldBe` Nothing

      it "hspec meta: `setupDB, teardownDB` are called once for every `it` here." $ \ st -> do
        uids <- evalLIO (queryLIO st AllUserIDs) allowAll
        uids `shouldBe` [UserId 0, UserId 1]

    describe "AddService, LookupService, DeleteService" $ do
      it "works" $ \st -> do
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
      it "works" $ \ st -> do
        from <- TimeStamp <$> getCurrentTime
        to <- TimeStamp <$> getCurrentTime
        evalLIO (updateLIO st (StartSession (UserId 0) "nosuchservice" from to)) allowAll `shouldThrow` anyException
        sid :: ServiceId <- evalLIO (updateLIO st AddService) allowAll
        evalLIO (updateLIO_ st (StartSession (UserId 0) sid from to)) allowAll

  describe "Api" . before setupTestServer . after teardownTestServer $ do
    describe "GET /user" $
      it "returns the list of users" $ \ (db, testServer) -> (debugRunSession False testServer) $ do
        response1 <- request $ defaultRequest
          { requestMethod = "GET"
          , pathInfo = ["user"]
          }
        liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 200
        liftIO $ Aeson.decode' (simpleBody response1) `shouldBe` Just [UserId 0, UserId 1]

    describe "POST /user" $
      it "succeeds" $ \ (db, testServer) -> (debugRunSession True testServer) $ do
        response2 <- srequest $ mkSRequest "POST" "/user" [] (Aeson.encode $ User "1" "2" "3" [] Nothing)
        liftIO $ C.statusCode (simpleStatus response2) `shouldBe` 201


-- * helpers

allowAll :: LIOState DCLabel
allowAll = LIOState (True %% True) (True %% True)

user1, user2, user3 :: User
user1 = User "name1" "passwd" "em@il" [] Nothing
user2 = User "name2" "passwd" "em@il" ["group1", "group2"] Nothing
user3 = User "name3" "3" "3" ["23"] Nothing

setupDB :: IO (AcidState DB)
setupDB = do
  st <- openLocalStateFrom (dbPath config) emptyDB
  evalLIO (updateLIO_ st $ AddUser user1) allowAll
  evalLIO (updateLIO_ st $ AddUser user2) allowAll
  return st

teardownDB :: AcidState DB -> IO ()
teardownDB st = do
  closeAcidState st
  removeTree $ fromString (dbPath config)

setupTestServer :: IO (AcidState DB, Application)
setupTestServer = do
  st <- setupDB
  let testServer = serve (Proxy :: Proxy App) (app st)
  return (st, testServer)

teardownTestServer :: (AcidState DB, Application) -> IO ()
teardownTestServer (st, testServer) = teardownDB st

-- | Cloned from hspec-wai's 'request'.  (We don't want to use the
-- return type from there.)
mkSRequest :: Method -> SBS -> [Header] -> LBS -> SRequest
mkSRequest method path headers body = SRequest req body
  where
    req = setPath defaultRequest {requestMethod = method, requestHeaders = headers} path

-- | Like `runSession`, but with re-ordered arguments, and with an
-- extra debug-output flag.
debugRunSession :: Bool -> Application -> Network.Wai.Test.Session a -> IO a
debugRunSession debug application session = runSession session (wrapApplication debug)
  where
    wrapApplication :: Bool -> Application
    wrapApplication False = application
    wrapApplication True = \ request respond -> do
      putStrLn $ showRequest request
      application request (\ response -> putStrLn (showResponse response)  >> respond response)

    showRequest request = showRequestHeader ++ ppShow request ++ body
      where
        showRequestHeader = "\n=== REQUEST ==========================================================\n"

        body = "\nbody:" ++ show ((cs . unsafePerformIO . requestBody $ request) :: LBS) ++ "\n"

    showResponse response = showResponseHeader ++ show_ response
      where
        showResponseHeader = "\n=== RESPONSE =========================================================\n"

        show_ :: Response -> String
        show_ (ResponseFile _ _ _ _) = "ResponseFile"
        show_ (ResponseBuilder status headers builder) = "ResponseBuilder" ++ show (status, headers)
        show_ (ResponseStream status headers (streamingBody :: StreamingBody)) = "ResponseStream" ++ show (status, headers)
        show_ (ResponseRaw _ _) = "ResponseRaw"

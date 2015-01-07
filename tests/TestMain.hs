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
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.String.Conversions (LBS, SBS, cs)
import Data.Thyme (getCurrentTime)
import Filesystem (removeTree)
import GHC.Exts (fromString)
import LIO.DCLabel (DCLabel, (%%))
import LIO (LIOState(LIOState), evalLIO)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Method (Method)
import Network.Wai (Application, StreamingBody, requestMethod, requestBody, strictRequestBody, pathInfo, requestHeaders)
import Network.Wai.Internal (Response(ResponseFile, ResponseBuilder, ResponseStream, ResponseRaw))
import Network.Wai.Test (Session, SRequest(SRequest),
    runSession, request, srequest, setPath, defaultRequest, simpleStatus, simpleBody)
import Servant.Server (serve)
import Test.Hspec (hspec, describe, it, before, after, shouldBe, shouldThrow,
    anyException, shouldSatisfy)
import Text.Show.Pretty (ppShow)

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
        uid <- evalLIO (updateLIO st $ AddUser user1) allowEverything
        Just user1' <- evalLIO (queryLIO st $ LookupUser uid) allowEverything
        user1' `shouldBe` user1
        evalLIO (updateLIO st $ DeleteUser (UserId 1)) allowEverything
        u <- evalLIO (queryLIO st $ LookupUser (UserId 1)) allowEverything
        u `shouldBe` Nothing

      it "hspec meta: `setupDB, teardownDB` are called once for every `it` here." $ \ st -> do
        uids <- evalLIO (queryLIO st AllUserIDs) allowEverything
        uids `shouldBe` [UserId 0, UserId 1]

    describe "AddService, LookupService, DeleteService" $ do
      it "works" $ \st -> do
        service1_id <- evalLIO (updateLIO st AddService) allowEverything
        service2_id <- evalLIO (updateLIO st AddService) allowEverything
        Just service1 <- evalLIO (queryLIO st $ LookupService service1_id) allowEverything
        Just service2 <- evalLIO (queryLIO st $ LookupService service2_id) allowEverything
        service1 `shouldBe` service1 -- sanity check for reflexivity of Eq
        service1 `shouldSatisfy` (/= service2) -- should have different keys
        evalLIO (updateLIO st $ DeleteService service1_id) allowEverything
        Nothing <- evalLIO (queryLIO st $ LookupService service1_id) allowEverything
        return ()

    describe "StartSession" $ do
      it "works" $ \ st -> do
        from <- TimeStamp <$> getCurrentTime
        to <- TimeStamp <$> getCurrentTime
        evalLIO (updateLIO st (StartSession (UserId 0) "nosuchservice" from to)) allowEverything `shouldThrow` anyException
        sid :: ServiceId <- evalLIO (updateLIO st AddService) allowEverything
        evalLIO (updateLIO_ st (StartSession (UserId 0) sid from to)) allowEverything

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
      it "succeeds" $ \ (db, testServer) -> (debugRunSession False testServer) $ do
        response2 <- srequest $ mkSRequest "POST" "/user" [] (Aeson.encode $ User "1" "2" "3" [] [])
        liftIO $ C.statusCode (simpleStatus response2) `shouldBe` 201


-- * helpers

user1, user2, user3 :: User
user1 = User "name1" "passwd" "em@il" [] []
user2 = User "name2" "passwd" "em@il" [("bal", ["group1"]), ("bla", ["group2"])] []
user3 = User "name3" "3" "3" [("bla", ["23"])] []

setupDB :: IO (AcidState DB)
setupDB = do
  st <- openLocalStateFrom (dbPath config) emptyDB
  evalLIO (updateLIO_ st $ AddUser user1) allowEverything
  evalLIO (updateLIO_ st $ AddUser user2) allowEverything
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
    req = setPath defaultRequest { requestMethod = method, requestHeaders = headers } path

-- | Like `runSession`, but with re-ordered arguments, and with an
-- extra debug-output flag.  It's not a pretty function, but it helps
-- with debugging, and it is not intended for production use.
debugRunSession :: Bool -> Application -> Network.Wai.Test.Session a -> IO a
debugRunSession debug application session = runSession session (wrapApplication debug)
  where
    wrapApplication :: Bool -> Application
    wrapApplication False = application
    wrapApplication True = \ request respond -> do
      (requestRendered, request') <- showRequest request
      print requestRendered
      application request' (\ response -> putStrLn (showResponse response)  >> respond response)

    showRequest request = do
        body :: LBS <- strictRequestBody request
        bodyRef :: IORef Bool <- newIORef False

        let memoBody = do
              toggle <- readIORef bodyRef
              writeIORef bodyRef $ not toggle
              return $ if toggle then "" else cs body

        let  showRequestHeader = "\n=== REQUEST ==========================================================\n"

             show_ :: LBS -> String
             show_ body = showRequestHeader ++ ppShow request ++ "\nbody:" ++ show body ++ "\n"

             request' = request { requestBody = memoBody }

        return (show_ body, request')
      where

    showResponse response = showResponseHeader ++ show_ response
      where
        showResponseHeader = "\n=== RESPONSE =========================================================\n"

        show_ :: Response -> String
        show_ (ResponseFile _ _ _ _) = "ResponseFile"
        show_ (ResponseBuilder status headers builder) = "ResponseBuilder" ++ show (status, headers)
        show_ (ResponseStream status headers (streamingBody :: StreamingBody)) = "ResponseStream" ++ show (status, headers)
        show_ (ResponseRaw _ _) = "ResponseRaw"

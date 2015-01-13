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
import Control.Monad (void)
import Data.Acid (AcidState, openLocalStateFrom, closeAcidState)
import Data.Acid.Advanced (query', update')
import Data.Data (Proxy(Proxy))
import Data.Functor.Infix ((<$>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.String.Conversions (LBS, SBS, cs)
import Data.Thyme (getCurrentTime)
import Filesystem (removeTree)
import GHC.Exts (fromString)
import LIO (canFlowTo)
import LIO.DCLabel ((%%), (/\), (\/), toCNF)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Method (Method)
import Network.Wai (Application, StreamingBody, requestMethod, requestBody, strictRequestBody, pathInfo, requestHeaders)
import Network.Wai.Internal (Response(ResponseFile, ResponseBuilder, ResponseStream, ResponseRaw))
import Network.Wai.Test (Session, SRequest(SRequest),
    runSession, request, srequest, setPath, defaultRequest, simpleStatus, simpleBody)
import Servant.Server (serve)
import Test.Hspec (hspec, describe, it, before, after, shouldBe,
    shouldSatisfy, pendingWith)
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
        Right uid <- update' st $ AddUser thentosPublic user1
        Right user1' <- query' st $ LookupUser thentosPublic uid
        user1' `shouldBe` user1
        void . update' st $ DeleteUser thentosPublic (UserId 1)
        u <- query' st $ LookupUser thentosPublic (UserId 1)
        u `shouldBe` Left NoSuchUser

      it "hspec meta: `setupDB, teardownDB` are called once for every `it` here." $ \ st -> do
        uids <- query' st $ AllUserIDs thentosPublic
        uids `shouldBe` Right [UserId 0, UserId 1]

    describe "AddService, LookupService, DeleteService" $ do
      it "works" $ \st -> do
        Right service1_id <- update' st $ AddService thentosPublic
        Right service2_id <- update' st $ AddService thentosPublic
        Right service1 <- query' st $ LookupService thentosPublic service1_id
        Right service2 <- query' st $ LookupService thentosPublic service2_id
        service1 `shouldBe` service1 -- sanity check for reflexivity of Eq
        service1 `shouldSatisfy` (/= service2) -- should have different keys
        void . update' st $ DeleteService thentosPublic service1_id
        Left NoSuchService <- query' st $ LookupService thentosPublic service1_id
        return ()

    describe "StartSession" $ do
      it "works" $ \ st -> do
        from <- TimeStamp <$> getCurrentTime
        to <- TimeStamp <$> getCurrentTime
        Left NoSuchService <- update' st $ StartSession thentosPublic (UserId 0) "NoSuchService" from to
        Right (sid :: ServiceId) <- update' st $ AddService thentosPublic
        Right _ <- update' st $ StartSession thentosPublic (UserId 0) sid from to
        return ()

  describe "Api" . before setupTestServer . after teardownTestServer $ do
    describe "authentication" $ do
      it "lets user view itself" $
          \ (_, testServer) -> (debugRunSession False testServer) $ do
        response1 <- srequest $ mkSRequest "GET" "/user/0" [("X-Principal", "0"), ("X-Password", "passwd")] ""
        liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 200

      it "responds with an error if clearance is insufficient" $
          \ (_, testServer) -> (debugRunSession False testServer) $ do
        liftIO $ pendingWith "not implemented yet"
        response1 <- srequest $ mkSRequest "GET" "/user/0" [] ""
        liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 303  -- FIXME: is that the expected response code?

      it "responds with an error if password is wrong" $
          \ (_, testServer) -> (debugRunSession False testServer) $ do
        liftIO $ pendingWith "not implemented yet"
        response1 <- srequest $ mkSRequest "GET" "/user/0" [("X-Principal", "0"), ("X-Password", "not-my-password")] ""
        liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 303  -- FIXME: is that the expected response code?

      it "responds with an error if only one of principal, password is provided" $
          \ (_, testServer) -> (debugRunSession False testServer) $ do
        liftIO $ pendingWith "not implemented yet"
        response1 <- srequest $ mkSRequest "GET" "/user/0" [("X-Principal", "0")] ""
        liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 303  -- FIXME: is that the expected response code?
        response2 <- srequest $ mkSRequest "GET" "/user/0" [("X-Password", "passwd")] ""
        liftIO $ C.statusCode (simpleStatus response2) `shouldBe` 303  -- FIXME: is that the expected response code?

      -- FIXME: we need to have a user whose password can be set in a
      -- configuration file.  that user has role "admin" and can do
      -- anything to anybody.  in particular, this user can create new
      -- users and services and assign roles.  (until we have a
      -- configuration file, the password can be configured in "DB",
      -- or somewhere.)
      it "special user admin can do anything" $
          \ (_, testServer) -> (debugRunSession True testServer) $ do
        liftIO $ pendingWith "not implemented yet"

    describe "GET /user" $
      it "returns the list of users" $
          \ (_, testServer) -> (debugRunSession False testServer) $ do
        response1 <- request $ defaultRequest
          { requestMethod = "GET"
          , pathInfo = ["user"]
          }
        liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 200
        liftIO $ Aeson.decode' (simpleBody response1) `shouldBe` Just [UserId 0, UserId 1]

    describe "POST /user" $
      it "succeeds" $
          \ (_, testServer) -> (debugRunSession False testServer) $ do
        response2 <- srequest $ mkSRequest "POST" "/user" [] (Aeson.encode $ User "1" "2" "3" [] [])
        liftIO $ C.statusCode (simpleStatus response2) `shouldBe` 201

  -- This test doesn't really test thentos code, but it helps
  -- understanding DCLabel.
  describe "DCLabel" $
    it "works" $ do
      let a = toCNF ("a" :: String)
          b = toCNF ("b" :: String)

      and [ b \/ a %% a `canFlowTo` a %% a
          , a %% a /\ b `canFlowTo` a %% a
          , True %% False `canFlowTo` False %% True
          , True %% True `canFlowTo` False %% True
          , False %% False `canFlowTo` False %% True
          , True
          ] `shouldBe` True


-- * helpers

user1, user2, user3 :: User
user1 = User "name1" "passwd" "em@il" [] []
user2 = User "name2" "passwd" "em@il" [("bal", ["group1"]), ("bla", ["group2"])] []
user3 = User "name3" "3" "3" [("bla", ["23"])] []

setupDB :: IO (AcidState DB)
setupDB = do
  st <- openLocalStateFrom (dbPath config) emptyDB
  void . update' st $ AddUser thentosPublic user1
  void . update' st $ AddUser thentosPublic user2
  return st

teardownDB :: AcidState DB -> IO ()
teardownDB st = do
  closeAcidState st
  removeTree $ fromString (dbPath config)

setupTestServer :: IO (AcidState DB, Application)
setupTestServer = do
  st <- setupDB
  return (st, serve (Proxy :: Proxy App) (app st))

teardownTestServer :: (AcidState DB, Application) -> IO ()
teardownTestServer (st, _) = teardownDB st

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
    wrapApplication True = \ _request respond -> do
      (requestRendered, request') <- showRequest _request
      print requestRendered
      application request' (\ response -> putStrLn (showResponse response)  >> respond response)

    showRequest _request = do
        body :: LBS <- strictRequestBody _request
        bodyRef :: IORef Bool <- newIORef False

        let memoBody = do
              toggle <- readIORef bodyRef
              writeIORef bodyRef $ not toggle
              return $ if toggle then "" else cs body

        let  showRequestHeader = "\n=== REQUEST ==========================================================\n"

             showBody :: String
             showBody = showRequestHeader ++ ppShow _request ++ "\nbody:" ++ show body ++ "\n"

             request' = _request { requestBody = memoBody }

        return (showBody, request')
      where

    showResponse response = showResponseHeader ++ show_ response
      where
        showResponseHeader = "\n=== RESPONSE =========================================================\n"

        show_ :: Response -> String
        show_ (ResponseFile _ _ _ _) = "ResponseFile"
        show_ (ResponseBuilder status headers _) = "ResponseBuilder" ++ show (status, headers)
        show_ (ResponseStream status headers (_ :: StreamingBody)) = "ResponseStream" ++ show (status, headers)
        show_ (ResponseRaw _ _) = "ResponseRaw"

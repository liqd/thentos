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

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad.State (liftIO)
import Control.Monad (void, when)
import Crypto.Random (SystemRNG, createEntropyPool, cprgCreate)
import Data.Acid (AcidState, openLocalStateFrom, closeAcidState)
import Data.Acid.Advanced (query', update')
import Data.Either (isLeft, isRight)
import Data.Functor.Infix ((<$>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.String.Conversions (LBS, SBS, cs)
import Data.Thyme (getCurrentTime)
import Filesystem (isDirectory, removeTree)
import GHC.Exts (fromString)
import LIO (canFlowTo)
import LIO.DCLabel ((%%), (/\), (\/), toCNF)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Method (Method)
import Network.Wai (Application, StreamingBody, requestMethod, requestBody, strictRequestBody, pathInfo, requestHeaders)
import Network.Wai.Internal (Response(ResponseFile, ResponseBuilder, ResponseStream, ResponseRaw))
import Network.Wai.Test (runSession, request, srequest, setPath, defaultRequest, simpleStatus, simpleBody)
import Network.Wai.Test (Session, SRequest(SRequest))
import Test.Hspec (hspec, describe, it, before, after, shouldBe, shouldSatisfy)
import Text.Show.Pretty (ppShow)

import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Types.Status as C

import Backend.Api.Simple
import DB
import Api
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
    describe "hspec meta" $ do
      it "`setupDB, teardownDB` are called once for every `it` here (part I)." $ \ (st, _) -> do
        Right _ <- update' st $ AddUser user3 allowEverything
        True `shouldBe` True

      it "`setupDB, teardownDB` are called once for every `it` here (part II)." $ \ (st, _) -> do
        uids <- query' st $ AllUserIds allowEverything
        uids `shouldBe` Right [UserId 0, UserId 1, UserId 2]  -- (no (UserId 2))

    describe "AddUser, LookupUser, DeleteUser" $ do
      it "works" $ \ (st, _) -> do
        Right uid <- update' st $ AddUser user3 allowEverything
        Right (uid', user3') <- query' st $ LookupUser uid allowEverything
        user3' `shouldBe` user3
        uid' `shouldBe` uid
        void . update' st $ DeleteUser uid allowEverything
        u <- query' st $ LookupUser uid allowEverything
        u `shouldBe` Left NoSuchUser

      it "guarantee that email addresses are unique" $ \ (st, _) -> do
        result <- update' st $ AddUser user1 allowEverything
        result `shouldBe` Left UserEmailAlreadyExists

    describe "DeleteUser" $ do
      it "user can delete herself, even if not admin" $ \ (st, _) -> do
        let uid = UserId 1
        result <- update' st $ DeleteUser uid (UserA uid *%% UserA uid)
        result `shouldBe` Right ()

      it "nobody else but the deleted user and admin can do this" $ \ (st, _) -> do
        result <- update' st $ DeleteUser (UserId 1) (UserA (UserId 2) *%% UserA (UserId 2))
        result `shouldSatisfy` isLeft

    describe "UpdateUser" $ do
      it "changes user if it exists" $ \ (st, _) -> do
        result <- update' st $ UpdateUser (UserId 1) user1 allowEverything
        result `shouldBe` Right ()
        result2 <- query' st $ LookupUser (UserId 1) allowEverything
        result2 `shouldBe` (Right (UserId 1, user1))

      it "throws an error if user does not exist" $ \ (st, _) -> do
        result <- update' st $ UpdateUser (UserId 391) user3 allowEverything
        result `shouldBe` Left NoSuchUser

    describe "AddUsers" $ do
      it "works" $ \ (st, _) -> do
        result <- update' st $ AddUsers [user3, user4, user5] allowEverything
        result `shouldBe` Right (map UserId [3, 4, 5])

      it "rolls back in case of error (adds all or nothing)" $ \ (st, _) -> do
        Left UserEmailAlreadyExists <- update' st $ AddUsers [user4, user3, user3] allowEverything
        result <- query' st $ AllUserIds allowEverything
        result `shouldBe` Right (map UserId [0, 1, 2])

    describe "AddService, LookupService, DeleteService" $ do
      it "works" $ \ (st, rng) -> do
        Right (service1_id, _s1_key) <- runAction' (st, allowEverything, rng) addService
        Right (service2_id, _s2_key) <- runAction' (st, allowEverything, rng) addService
        Right service1 <- query' st $ LookupService service1_id allowEverything
        Right service2 <- query' st $ LookupService service2_id allowEverything
        service1 `shouldBe` service1 -- sanity check for reflexivity of Eq
        service1 `shouldSatisfy` (/= service2) -- should have different keys
        void . update' st $ DeleteService service1_id allowEverything
        Left NoSuchService <- query' st $ LookupService service1_id allowEverything
        return ()

    describe "StartSession" $ do
      it "works" $ \ (st, rng) -> do
        from <- TimeStamp <$> getCurrentTime
        let timeout = Timeout 600
        Left NoSuchService <- runAction' (st, allowEverything, rng) $ startSession (UserId 0) "NoSuchService" from timeout
        Right (sid :: ServiceId, _) <- runAction' (st, allowEverything, rng) $ addService
        Right _ <- runAction' (st, allowEverything, rng) $ startSession (UserId 0) sid from timeout
        return ()

    describe "agents and roles" $ do
      describe "assign" $ do
        it "can be called by admins" $ \ (st, _) -> do
          let targetAgent = UserA $ UserId 1
          result <- update' st $ AssignRole targetAgent RoleAdmin (RoleAdmin *%% RoleAdmin)
          result `shouldSatisfy` isRight

        it "can NOT be called by any non-admin agents" $ \ (st, _) -> do
          let targetAgent = UserA $ UserId 1
          result <- update' st $ AssignRole targetAgent RoleAdmin (targetAgent *%% targetAgent)
          result `shouldSatisfy` isLeft

      describe "lookup" $ do
        it "can be called by admins" $ \ (st, _) -> do
          let targetAgent = UserA $ UserId 1
          result :: Either DbError [Role] <- query' st $ LookupAgentRoles targetAgent (RoleAdmin *%% RoleAdmin)
          result `shouldSatisfy` isRight

        it "can be called by user for her own roles" $ \ (st, _) -> do
          let targetAgent = UserA $ UserId 1
          result <- query' st $ LookupAgentRoles targetAgent (targetAgent *%% targetAgent)
          result `shouldSatisfy` isRight

        it "can NOT be called by other users" $ \ (st, _) -> do
          let targetAgent = UserA $ UserId 1
              askingAgent = UserA $ UserId 2
          result <- query' st $ LookupAgentRoles targetAgent (askingAgent *%% askingAgent)
          result `shouldSatisfy` isLeft

  describe "Api" . before setupTestServer . after teardownTestServer $ do
    describe "authentication" $ do
      it "lets user view itself" $
          \ (_, testServer) -> (debugRunSession False testServer) $ do
        response1 <- srequest $ mkSRequest "GET" "/user/0" godCredentials ""
        liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 200

      it "responds with an error if clearance is insufficient" $
          \ (_, testServer) -> (debugRunSession False testServer) $ do
        response1 <- srequest $ mkSRequest "GET" "/user/0" [] ""
        liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 401

      it "responds with an error if password is wrong" $
          \ (_, testServer) -> (debugRunSession False testServer) $ do
        response1 <- srequest $ mkSRequest "GET" "/user/0" [("X-Thentos-User", "god"), ("X-Thentos-Password", "not-gods-password")] ""
        liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 401

      it "responds with an error if only one of user (or service) and password is provided" $
          \ (_, testServer) -> (debugRunSession False testServer) $ do
        response1 <- srequest $ mkSRequest "GET" "/user/0" [("X-Thentos-User", "god")] ""
        liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 400
        response2 <- srequest $ mkSRequest "GET" "/user/0" [("X-Thentos-Service", "dog")] ""
        liftIO $ C.statusCode (simpleStatus response2) `shouldBe` 400
        response3 <- srequest $ mkSRequest "GET" "/user/0" [("X-Thentos-Password", "passwd")] ""
        liftIO $ C.statusCode (simpleStatus response3) `shouldBe` 400
        response4 <- srequest $ mkSRequest "GET" "/user/0" [("X-Thentos-User", "god"), ("X-Thentos-Service", "dog")] ""
        liftIO $ C.statusCode (simpleStatus response4) `shouldBe` 400

    describe "GET /user" $
      it "returns the list of users" $
          \ (_, testServer) -> (debugRunSession False testServer) $ do
        response1 <- request $ defaultRequest
          { requestMethod = "GET"
          , requestHeaders = godCredentials
          , pathInfo = ["user"]
          }
        liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 200
        liftIO $ Aeson.decode' (simpleBody response1) `shouldBe` Just [UserId 0, UserId 1, UserId 2]

    describe "POST /user" $
      it "succeeds" $
          \ (_, testServer) -> (debugRunSession False testServer) $ do
        response2 <- srequest $ mkSRequest "POST" "/user" godCredentials (Aeson.encode $ User "1" "2" "3" [] [])
        liftIO $ C.statusCode (simpleStatus response2) `shouldBe` 201

  -- This test doesn't really test thentos code, but it helps
  -- understanding DCLabel.
  describe "DCLabel" $
    it "works" $ do
      let a = toCNF ("a" :: String)
          b = toCNF ("b" :: String)
          c = toCNF ("c" :: String)

      and [ b \/ a %% a `canFlowTo` a %% a
          , a %% a /\ b `canFlowTo` a %% a
          , a %% (a /\ b) \/ (a /\ c) `canFlowTo` a %% a
          , not $ a %% (a /\ b) \/ (a /\ c) `canFlowTo` a %% b
          ,       True  %% False `canFlowTo` True %% False
          ,       True  %% False `canFlowTo` False %% True
          ,       True  %% True  `canFlowTo` False %% True
          ,       False %% False `canFlowTo` False %% True
          , not $ False %% True  `canFlowTo` True  %% False
          , not $ True  %% True  `canFlowTo` True  %% False
          , not $ False %% False `canFlowTo` True  %% False
          , True
          ] `shouldBe` True


-- * helpers

user1, user2, user3, user4, user5 :: User
user1 = User "name1" "passwd" "em@il" [] []
user2 = User "name2" "passwd" "em38@il" [("bal", ["group1"]), ("bla", ["group2"])] []
user3 = User "name3" "3" "3" [("bla", ["23"])] []
user4 = User "name4" "4" "4" [] []
user5 = User "name5" "5" "5" [] []

destroyDB :: IO ()
destroyDB = do
  let p = (fromString (dbPath config))
    in isDirectory p >>= \ yes -> when yes $ removeTree p

setupDB :: IO (AcidState DB, MVar SystemRNG)
setupDB = do
  destroyDB
  st <- openLocalStateFrom (dbPath config) emptyDB
  createGod st False
  Right (UserId 1) <- update' st $ AddUser user1 allowEverything
  Right (UserId 2) <- update' st $ AddUser user2 allowEverything
  rng :: MVar SystemRNG <- createEntropyPool >>= newMVar . cprgCreate
  return (st, rng)

teardownDB :: (AcidState DB, MVar SystemRNG) -> IO ()
teardownDB (st, _) = do
  closeAcidState st
  destroyDB

setupTestServer :: IO ((AcidState DB, MVar SystemRNG), Application)
setupTestServer = do
  (st, rng) <- setupDB
  return ((st, rng), serveApi (st, rng))

teardownTestServer :: ((AcidState DB, MVar SystemRNG), Application) -> IO ()
teardownTestServer (db, _) = teardownDB db

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

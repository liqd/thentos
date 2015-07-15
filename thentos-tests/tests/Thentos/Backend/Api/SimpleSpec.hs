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

module Thentos.Backend.Api.SimpleSpec
where

import Control.Lens ((^.))
import Control.Monad.State (liftIO)
import Control.Monad (void)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Network.Wai.Test (srequest, simpleStatus, simpleBody)
import Test.Hspec (Spec, describe, it, before, after, shouldBe, pendingWith, hspec)

import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Types.Status as C

import Thentos.Types
import Thentos.Backend.Api.Simple (serveApi)

import Thentos.Test.Config
import Thentos.Test.Types
import Thentos.Test.Core hiding (setupTestBackend)
import qualified Thentos.Test.Core (setupTestBackend)

setupTestBackend :: IO (BTS DB)
setupTestBackend = Thentos.Test.Core.setupTestBackend serveApi


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = do
    describe "Fixtures" $ do
        it "make sure backend is running at all." $
            void $ setupTestBackend

        it "tear it down again, too." $
            void $ setupTestBackend >>= teardownTestBackend

    describe "Thentos.Backend.Api.Simple" . before setupTestBackend . after teardownTestBackend $ do
        describe "headers" $ do
            it "bad unknown headers matching /X-Thentos-*/ yields an error response." $
              \ (bts :: (BTS DB)) -> runTestBackend bts $ do
                let headers = ("X-Thentos-No-Such-Header", "3"):(bts ^. btsGodCredentials)
                let req = makeSRequest "GET" "/user" headers ""
                resp <- srequest req
                liftIO $ C.statusCode (simpleStatus resp) `shouldBe` 400

        describe "user" $ do
            describe "Get [UserId]" $ do
                it "returns the list of users" $
                  \ bts -> runTestBackend bts $ do
                    response1 <- srequest $ makeSRequest "GET" "/user" (bts ^. btsGodCredentials) ""
                    liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 200
                    liftIO $ Aeson.decode' (simpleBody response1) `shouldBe` Just [UserId 0]

                it "is not accessible for users without 'Admin' role" $
                  \ bts -> runTestBackend bts $ do
                    response1 <- srequest $ makeSRequest "GET" "/user" [] ""
                    liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 401

            describe "Capture \"userid\" UserId :> \"name\" :> Get UserName" $ do
                let resource = "/user/0/name"
                it "yields a name" $
                  \ bts -> runTestBackend bts $ do
                    response1 <- srequest $ makeSRequest "GET" resource (bts ^. btsGodCredentials) ""
                    liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 200

                it "can be called by user herself" $
                        \ _ -> pendingWith "test missing."

                it "can be called by admin" $
                        \ _ -> pendingWith "test missing."

                it "can not be callbed by other (non-admin) users" $
                        \ _ -> pendingWith "test missing."

                it "responds with an error if password is wrong" $
                        \ _ -> pendingWith "test missing."

            describe "Capture \"userid\" UserId :> \"email\" :> Get UserEmail" $ do
                let resource = "/user/0/email"
                it "yields an email address" $
                  \ bts -> runTestBackend bts $ do
                    response1 <- srequest $ makeSRequest "GET" resource (bts ^. btsGodCredentials) ""
                    liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 200


            describe "ReqBody UserFormData :> Post UserId" $ do
                it "writes a new user to the database" $
                  \ bts -> runTestBackend bts $ do
                    let userData = UserFormData "1" "2" $ forceUserEmail "somebody@example.org"
                    response1 <- srequest $ makeSRequest "POST" "/user" (bts ^. btsGodCredentials) (Aeson.encode userData)
                    liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 201
                    let uid = case fmap UserId . decodeLenient $ simpleBody response1 of
                          Right v -> v
                          Left e -> error $ show (e, response1)
                    response2 <- srequest $ makeSRequest "GET"
                                    ("/user/" <> (cs . show . fromUserId $ uid) <> "/name")
                                    (bts ^. btsGodCredentials) ""
                    let name = case decodeLenient $ simpleBody response2 of
                          Right v -> v
                          Left e -> error $ show ("/user/" ++ show uid, e, response1)
                    liftIO $ name `shouldBe` udName userData

                it "can only be called by admins" $
                        \ _ -> pendingWith "test missing."

            describe "Capture \"userid\" UserId :> ReqBody User :> Put ()" $ do
                it "writes an *existing* user to the database" $
                        \ _ -> pendingWith "test missing."
                    -- put user' with user' /= user
                    -- lookup user id
                    -- compare sent and received

                it "can only be called by admins and the user herself" $
                        \ _ -> pendingWith "test missing."

                it "if user does not exist, responds with an error" $
                        \ _ -> pendingWith "test missing."

            describe "Capture \"userid\" UserId :> Delete" $ do
                it "removes an existing user from the database" $
                        \ _ -> pendingWith "test missing."

                it "can only be called by admins and the user herself" $
                        \ _ -> pendingWith "test missing."

                it "if user does not exist, responds with an error" $
                        \ _ -> pendingWith "test missing."


        describe "service" $ do

            describe "Get [ServiceId]" $ do
                it "..." $ \ _ -> pendingWith "no tests yet"

            describe "Capture \"sid\" ServiceId :> Get (ServiceId, Service)" $ do
                it "..." $ \ _ -> pendingWith "no tests yet"

            describe "Post (ServiceId, ServiceKey)" $ do
                it "..." $ \ _ -> pendingWith "no tests yet"


        describe "session" $ do

            describe "ReqBody (UserId, ServiceId) :> Post SessionToken" $ do
                it "starts a new session and returns the session token" $
                        \ _ -> pendingWith "no tests yet"

                it "sends a meaningful error message if request body is empty" $
                        \ _ -> pendingWith "no tests yet"

            describe "ReqBody (UserId, ServiceId, Timeout) :> Post SessionToken" $ do
                it "..." $ \ _ -> pendingWith "no tests yet"

            describe "Capture \"token\" SessionToken :> Delete" $ do
                it "..." $ \ _ -> pendingWith "no tests yet"

            describe "Capture \"token\" SessionToken :> Get Bool" $ do
                it "returns true if session is active" $
                        \ _ -> pendingWith "no tests yet"

                it "returns false if session is not active (or does not exist)" $
                        \ _ -> pendingWith "no tests yet"

                it "does not accept the empty string (trailing '/') as session id." $
                        \ _ -> pendingWith "not implemented"

                    -- (currently, GET /session and GET /session/
                    -- produce different error messages.  this is just
                    -- a matter of error reporting, though; the
                    -- behaviour in the context of correct requests is
                    -- as we want it.)

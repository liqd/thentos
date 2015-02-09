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

module Test.Thentos.Backend.Api.Simple
where

import Control.Monad.State (liftIO)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Network.Wai.Test (srequest, simpleStatus, simpleBody)
import Test.Hspec (Spec, describe, it, before, after, shouldBe, pendingWith)

import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Types.Status as C

import Types

import Test.Util


tests :: Spec
tests = do
    describe "Backend.Api.Simple" . before setupTestServer . after teardownTestServer $ do
        describe "user" $ do
            describe "Get [UserId]" $ do
                it "returns the list of users" $
                        \ (_, testServer) -> debugRunSession False testServer $ do
                    response1 <- srequest $ mkSRequest "GET" "/user" godCredentials ""
                    liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 200
                    liftIO $ Aeson.decode' (simpleBody response1) `shouldBe` Just [UserId 0, UserId 1, UserId 2]
                it "is not accessible for users without 'Admin' role" $
                        \ (_, testServer) -> debugRunSession False testServer $ do
                    response1 <- srequest $ mkSRequest "GET" "/user" [] ""
                    liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 401

            describe "Capture \"userid\" UserId :> Get (UserId, User)" $ do
                it "yields a user value" $
                        \ (_, testServer) -> (debugRunSession False testServer) $ do
                    response1 <- srequest $ mkSRequest "GET" "/user/0" godCredentials ""
                    liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 200

                it "can be called by user herself" $
                        \ _ -> pendingWith "test missing."

                it "can be called by admin" $
                        \ _ -> pendingWith "test missing."

                it "can not be callbed by other (non-admin) users" $
                        \ _ -> pendingWith "test missing."

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

            describe "ReqBody User :> Post UserId" $ do
                it "writes a new user to the database" $
                        \ (_, testServer) -> (debugRunSession False testServer) $ do
                    let user = User "1" "2" "3" [] []
                    response1 <- srequest $ mkSRequest "POST" "/user" godCredentials (Aeson.encode user)
                    liftIO $ C.statusCode (simpleStatus response1) `shouldBe` 201
                    let uid = case fmap UserId . decodeLenient $ simpleBody response1 of
                          Right v -> v
                          Left e -> error $ show (e, response1)

                    response2 <- srequest $ mkSRequest "GET" ("/user/" <> (cs . show . fromUserId $ uid)) godCredentials ""
                    let (uid', user') = case decodeLenient $ simpleBody response2 of
                          Right v -> v
                          Left e -> error $ show ("/user/" ++ show uid, e, response1)
                    liftIO $ uid' `shouldBe` uid
                    liftIO $ user' `shouldBe` user

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

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
import Network.Wai (requestMethod, pathInfo, requestHeaders)
import Network.Wai.Test (request, srequest, defaultRequest, simpleStatus, simpleBody)
import Test.Hspec (Spec, describe, it, before, after, shouldBe)

import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Types.Status as C

import DB
import Types

import Test.Util


tests :: Spec
tests = do
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

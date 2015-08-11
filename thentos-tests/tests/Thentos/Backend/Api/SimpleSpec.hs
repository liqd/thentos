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

module Thentos.Backend.Api.SimpleSpec (spec, tests)
where

import Control.Monad.State (liftIO)
import Data.Monoid ((<>))
import Data.IORef
import Data.String.Conversions (cs)
import Network.Wai (Application)
import Network.Wai.Test (simpleStatus, simpleBody)
import Network.HTTP.Types.Header (Header)
import Test.Hspec (Spec, describe, it, shouldBe, pendingWith, hspec)
import Test.Hspec.Wai (shouldRespondWith, with, get, request)

import qualified Data.Aeson as Aeson
import System.IO.Unsafe (unsafePerformIO)
import Network.HTTP.Types.Status ()

import Thentos.Backend.Api.Simple (serveApi)
import Thentos.Types
import Thentos.Action.Core

import Thentos.Test.Core
import Thentos.Test.Config

import Debug.Trace

defaultApp :: IO Application
defaultApp = do
    db@(ActionState (adb, _, _)) <- createActionState =<< thentosTestConfig
    createGod adb
    writeIORef godHeaders . snd =<< loginAsGod db
    let app = serveApi db
    return app

tests :: IO ()
tests = hspec spec

godHeaders :: IORef [Header]
godHeaders = unsafePerformIO $ newIORef []
{-# NOINLINE godHeaders #-}

spec :: Spec
spec = do

    with defaultApp  $ describe "Thentos.Backend.Api.Simple" $  do
        describe "headers" $ do
            it "bad unknown headers matching /X-Thentos-*/ yields an error response." $ do
                hdr <- liftIO $ readIORef godHeaders
                let headers = ("X-Thentos-No-Such-Header", "3"):hdr
                request "GET" "/user" headers "" `shouldRespondWith` 400

        describe "user" $ do
            describe "Get [UserId]" $ do
                it "returns the list of users" $ do
                    hdr <- liftIO $ readIORef godHeaders
                    response <- request "GET" "/user" hdr ""
                    return response `shouldRespondWith` 200
                    liftIO $ Aeson.decode' (simpleBody response) `shouldBe` Just [UserId 0]

                it "is not accessible for users without 'Admin' role" $ do
                    get "/user" `shouldRespondWith` 401

            describe "Capture \"userid\" UserId :> \"name\" :> Get UserName" $ do
                let resource = "/user/0/name"
                it "yields a name" $ do
                    hdr <- liftIO $ readIORef godHeaders
                    request "GET" resource hdr "" `shouldRespondWith` 200

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
                it "yields an email address" $ do
                    hdr <- liftIO $ readIORef godHeaders
                    request "GET" resource hdr "" `shouldRespondWith` 200


            describe "ReqBody UserFormData :> Post UserId" $ do
                it "writes a new user to the database" $ do
                    hdr <- liftIO $ readIORef godHeaders
                    let hdr' = ("Content-Type", "application/json"):hdr
                    let userData = UserFormData "1" "2" $ forceUserEmail "somebody@example.org"
                    request "POST" "/user" hdr' (Aeson.encode userData)
                        `shouldRespondWith` 201

                    let Right uid = decodeLenient $ simpleBody response1
                    response2 <- request "GET" ("/user" <> (cs . show $ fromUserId uid) <> "/name") hdr ""

                    let Right name = decodeLenient $ simpleBody response2
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

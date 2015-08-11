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

import Control.Applicative ((<$>))
import Control.Monad (void)
import Control.Monad.State (liftIO)
import Data.Monoid ((<>))
import Data.IORef
import Data.String.Conversions (cs)
import Network.Wai (Application)
import Network.Wai.Test (simpleBody, SResponse)
import Network.HTTP.Types.Header (Header)
import Test.Hspec (Spec, describe, it, shouldBe, pendingWith, hspec)
import Test.Hspec.Wai (shouldRespondWith, WaiSession, with, get, request, matchStatus)

import qualified Data.Aeson as Aeson
import System.IO.Unsafe (unsafePerformIO)
import Network.HTTP.Types.Status ()

import Thentos.Backend.Api.Simple (serveApi)
import Thentos.Types
import Thentos.Action.Core

import Thentos.Test.Core
import Thentos.Test.Config


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
                hdr <- liftIO ctHeader
                let headers = ("X-Thentos-No-Such-Header", "3"):hdr
                request "GET" "/user" headers "" `shouldRespondWith` 400

        describe "user" $ do
            describe "Get [UserId]" $ do
                it "returns the list of users" $ do
                    hdr <- liftIO ctHeader
                    response <- request "GET" "/user" hdr ""
                    return response `shouldRespondWith` 200
                    liftIO $ Aeson.decode' (simpleBody response) `shouldBe` Just [UserId 0]

                it "is not accessible for users without 'Admin' role" $ do
                    get "/user" `shouldRespondWith` 401

            describe "Capture \"userid\" UserId :> \"name\" :> Get UserName" $ do
                let resource = "/user/0/name"
                it "yields a name" $ do
                    hdr <- liftIO ctHeader
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
                    hdr <- liftIO ctHeader
                    request "GET" resource hdr "" `shouldRespondWith` 200


            describe "ReqBody UserFormData :> Post UserId" $ do
                it "writes a new user to the database" $ do
                    hdr <- liftIO ctHeader
                    response1 <- postUser
                    return response1 `shouldRespondWith` 201

                    let (uid :: Int) = read . cs $ simpleBody response1
                    response2 <- request "GET" ("/user/" <> (cs . show $ uid) <> "/name") hdr ""

                    let Right name = decodeLenient $ simpleBody response2
                    liftIO $ name `shouldBe` udName defaultUserData

                it "can only be called by admins" $
                        \ _ -> pendingWith "test missing."


            describe "Capture \"userid\" UserId :> Delete" $ do
                it "removes an existing user from the database" $ do
                    hdr <- liftIO ctHeader
                    let hdr' = ("Content-Type", "application/json"):hdr
                    let userData = UserFormData "1" "2" $ forceUserEmail "somebody@example.org"
                    response1 <- request "POST" "/user" hdr' (Aeson.encode userData)
                    let (uid :: Int) = read . cs $ simpleBody response1
                    request "GET" ("/user/" <> (cs . show $ uid) <> "/name") hdr ""
                        `shouldRespondWith` 200
                    void $ request "DELETE" ("/user/" <> cs (show uid)) hdr ""
                    request "GET" ("/user/" <> cs (show uid) <> "/name") hdr ""
                        `shouldRespondWith` 404

                it "can only be called by admins and the user herself" $
                        \ _ -> pendingWith "test missing."

                it "if user does not exist, responds with a 404" $ do
                    hdr <- liftIO ctHeader
                    request "DELETE" "/user/1797" hdr "" `shouldRespondWith` 404


        describe "thentos_session" $ do

            describe "Capture \"token\" ThentosSessionToken :> Get Bool" $ do
                it "returns true if session is active" $ do
                    hdr <- liftIO ctHeader
                    response1 <- postUser
                    let (uid :: Int) = read . cs $ simpleBody response1
                    response2 <- request "POST" "/thentos_session" hdr (Aeson.encode (uid, udPassword defaultUserData))
                    request "GET" "/thentos_session/" hdr (simpleBody response2)
                        `shouldRespondWith` "true" { matchStatus = 200 }

                it "returns false if session is not active (or does not exist)" $
                        \ _ -> pendingWith "no tests yet"

                it "does not accept the empty string (trailing '/') as session id." $
                        \ _ -> pendingWith "not implemented"

                    -- (currently, GET /session and GET /session/
                    -- produce different error messages.  this is just
                    -- a matter of error reporting, though; the
                    -- behaviour in the context of correct requests is
                    -- as we want it.)

postUser :: WaiSession SResponse
postUser = do
    hdr <- liftIO ctHeader
    request "POST" "/user" hdr (Aeson.encode defaultUserData)

ctHeader :: IO [Header]
ctHeader = (("Content-Type", "application/json") :) <$> readIORef godHeaders

defaultUserData :: UserFormData
defaultUserData = UserFormData "name" "pwd" $ forceUserEmail "somebody@example.org"

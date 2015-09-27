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

module Thentos.Backend.Api.SsoSpec (spec, tests)
where

import Control.Monad (void)
import Control.Monad.State (liftIO)
import Data.Monoid ((<>))
import Data.Pool (withResource)
import Data.IORef
import Data.String.Conversions (cs)
import Network.Wai (Application)
import Network.Wai.Test (simpleBody, SResponse)
import Network.HTTP.Types.Header (Header)
import Test.Hspec (Spec, describe, it, shouldBe, pendingWith, hspec)
import Test.Hspec.Wai (shouldRespondWith, WaiSession, with, request, matchStatus)

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
    db@(ActionState (connPool, _, _)) <- createActionState "test_thentos" thentosTestConfig
    return $! serveApi db

ctHeader :: [Header]
ctHeader = [("Content-Type", "application/json")]

tests :: IO ()
tests = hspec spec

spec :: Spec
spec = do
    with defaultApp $ describe "Thentos.Backend.Api.Sso" $ do
        describe "/sso/github/callbacks.js" $ do
            it "returns a javascript source file" $ do
                body :: ST <- request "GET" "/sso/github/callbacks.js" ctHeaders ""
                body `shouldContain` "var requestSso = function"
                body `shouldContain` "var confirmSso = function"

        describe "loginGithubUser" $ do
            describe "returns a valid thentos session token " $ do
                let ghUser = GithubUser 3 "bloob" "bloob@nowhere.noop"

                it "for *non-existing* user" $ do
                    tok <- loginGithubUser ghUser
                    ...  -- check user record in db

                it "for *existing* user" $ do
                    tok <- loginGithubUser ghUser
                    ...  -- check user record in db

        describe "/sso/github/request" $ do
            it "creates a new sso token in the database" $ do
                pending
            it "uses newly created sso token in its output" $ do
                pending
            it "uses the return uri in its output" $ do
                pending

        -- NOTE: these tests require reachability of `https://api.github.com/`
        describe "/sso/github/confirm" $ do
            it "when passed *illegal* state/code pair, throws an error" $ do
                pending
            it "when passed *legal* state/code pair, returns a valid thentos session token" $ do
                pending

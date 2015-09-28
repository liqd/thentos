{-# LANGUAGE DataKinds                                #-}
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

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad.State (liftIO)
import Control.Monad (void)
import Data.Configifier (Tagged(Tagged), (>>.))
import Data.IORef
import Data.Monoid ((<>))
import Data.Pool (withResource)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs)
import Network.HTTP.Types.Header (Header)
import Network.Wai (Application)
import Network.Wai.Test (simpleBody, SResponse)
import Servant.Server (ServerT, Server, serve, enter)
import Test.Hspec (Spec, SpecWith, beforeAll, describe, it, shouldBe, pending, hspec, shouldContain)

import qualified Data.Aeson as Aeson
import System.IO.Unsafe (unsafePerformIO)
import Network.HTTP.Types.Status ()

import Thentos.Action.Core
import Thentos.Backend.Api.Sso
import Thentos.Backend.Core
import Thentos.Config (ThentosConfig)
import Thentos.Types

import Thentos.Test.Core
import Thentos.Test.Config


setupSso :: ThentosConfig -> SpecWith ActionState
setupSso cfg = do
    let dbname = "test_thentos_sso"
        serveApi = serve (Proxy :: Proxy ThentosSso) . thentosSso
        Just beConfig = Tagged <$> cfg >>. (Proxy :: Proxy '["backend"])

    st@(ActionState (connPool, _, _))
        <- liftIO . createActionState dbname $ cfg
    backendThread
        <- liftIO . forkIO . runWarpWithCfg beConfig . serveApi $ st

    return backendThread

ctHeaders :: [Header]
ctHeaders = [("Content-Type", "application/json")]

tests :: IO ()
tests = hspec spec

spec :: Spec
spec = do
    beforeAll (setupSso thentosTestConfig) $ describe "Thentos.Backend.Api.Sso" $ do
        describe "/sso/github/callbacks.js" $ do
            it "returns a javascript source file" $ \(_, s) -> do
                body :: ST <- request "GET" "/sso/github/callbacks.js" ctHeaders ""
                body `shouldContain` "var requestSso = function"
                body `shouldContain` "var confirmSso = function"

        describe "loginGithubUser" $ do
            describe "returns a valid thentos session token " $ do
                let ghUser = GithubUser 3 "bloob" "bloob@nowhere.noop"

                it "for *non-existing* user" $ do
                    pending

                it "for *existing* user" $ do
                    pending

        describe "/sso/github/request" $ do
            it "creates a new sso token in the database" $ do
                pending
            it "uses newly created sso token in its output" $ do
                pending
            it "uses the return uri in its output" $ do
                pending

        -- NOTE: these tests require reachability of `https://api.github.com/`
        describe "/sso/github/confirm" $ do
            it "when passed non-existing sso token (state), throws an error" $ do
                pending
            it "when passed a code not provided by github, throws an error" $ do
                pending
            describe "when called with valid arguments" $ do
                it "returns a valid thentos session token" $ do
                    pending
                it "removes the sso token from the database" $ do
                    pending

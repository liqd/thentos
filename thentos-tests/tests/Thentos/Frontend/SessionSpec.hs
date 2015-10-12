{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Thentos.Frontend.SessionSpec where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Data.Pool (Pool, withResource)
import Data.String.Conversions (ST, cs)
import Data.Void (Void)
import Database.PostgreSQL.Simple (Connection)
import Test.Hspec (Spec, SpecWith, around, describe, it, shouldBe, shouldSatisfy, hspec, pendingWith)
import Test.Hspec.Wai
import Network.Wai
import Network.Wai.Test (simpleBody)
import Servant
import Data.Proxy
import Network.HTTP.Types

import qualified Data.Text as ST
import qualified Network.HTTP.Types.Status as C
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Class as WD

import Thentos.Action.Core
import Thentos.Config
import qualified Thentos.Transaction as T
import Thentos.Transaction.Core (ThentosQuery, runThentosQuery)
import Thentos.Types
import Thentos.Util ((<//>), verifyPass)

import Thentos.Test.WebDriver.Missing as WD
import Thentos.Test.Arbitrary ()
import Thentos.Test.Config
import Thentos.Test.Core

import Thentos.Frontend.Session
import Data.Aeson

tests :: IO ()
tests = hspec spec

server1 :: Application
server1 = serve (Proxy :: Proxy (Session :> Get '[JSON] Token)) return

spec :: Spec
spec = describe "asdf-session-tests" . with (return server1) $ do
    it "gets the token from the client cookie" $ do
        request methodGet "" [("Cookie", "bla")] "" `shouldRespondWith` "\"bla\""

    it "if no cookie is set, returns a fresh one" $ do
        resp <- request methodGet "" [] ""
        liftIO $ print (simpleBody resp)
        liftIO $ (decode $ simpleBody resp) `shouldSatisfy` (\(Just (Token _)) -> True)  -- fails because of missing leniency

    it "if no cookie is set, a fresh will be in the Set-Cookie header of the response" $ do
        pending

    it "cookies are fresh" $ do
        pending

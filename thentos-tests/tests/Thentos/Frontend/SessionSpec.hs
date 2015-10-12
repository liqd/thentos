{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Thentos.FrontendSpec where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Data.Pool (Pool, withResource)
import Data.String.Conversions (ST, cs)
import Data.Void (Void)
import Database.PostgreSQL.Simple (Connection)
import Test.Hspec (Spec, SpecWith, around, describe, it, shouldBe, shouldSatisfy, hspec, pendingWith)

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


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "session tests" $ do
  describe "many tests" . around (withFrontendAndBackend "test_thentos") $ do
    spec_createUser
    spec_resetPassword
    spec_logIntoThentos
    spec_logOutOfThentos

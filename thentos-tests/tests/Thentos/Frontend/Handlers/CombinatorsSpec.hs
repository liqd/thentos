{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Thentos.Frontend.Handlers.CombinatorsSpec where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Data.String.Conversions (ST, cs)
import Test.Hspec (Spec, SpecWith, around, describe, it, shouldBe, shouldSatisfy, hspec, pending)

import qualified Data.Text as ST
import qualified Network.HTTP.Types.Status as C
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Class as WD

import Thentos.Action.Core
import Thentos.Config
import qualified Thentos.Transaction as T
import Thentos.Types
import Thentos.Util

import Thentos.Test.WebDriver.Missing as WD
import Thentos.Test.Arbitrary ()
import Thentos.Test.Config
import Thentos.Test.Core
import Thentos.Test.Transaction


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "Thentos.Frontend.Handlers.CombinatorsSpec" $ do
    describe "redirect'" $ do
        it "redirects" $ do
            pending

 -- more test functionality from StateSpec into core?  nah, we probably need something else here...

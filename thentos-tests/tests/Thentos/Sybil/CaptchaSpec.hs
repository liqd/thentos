{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE QuasiQuotes                              #-}
{-# LANGUAGE ScopedTypeVariables                      #-}

module Thentos.Sybil.CaptchaSpec where

import Data.Aeson (decode, FromJSON)
import Data.Pool (Pool)
import Data.String.Conversions (cs)
import Database.PostgreSQL.Simple (Connection, Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics (Generic)
import LIO (canFlowTo, lub, glb)
import LIO.DCLabel (DCLabel, (%%), (/\), (\/), toCNF)
import Test.Hspec.QuickCheck (modifyMaxSize)
import Test.Hspec (Spec, SpecWith, before, context, describe, it, shouldBe, shouldNotBe)
import Test.QuickCheck (property)

import Thentos.Sybil.Captcha
import Thentos.Types

import Thentos.Test.Arbitrary ()
import Thentos.Test.Core
import Thentos.Test.Transaction

spec :: Spec
spec = describe "Thentos.Sybil.Captcha" $ do
    it "produces three-word phrases as solutions" $ do
        let Just x = generateCaptcha <$> mkRandom20 "--------------------"
        (length . words . cs . snd $ x) `shouldBe` 3

    it "different rnd seed produces different solutions" $ do
        let Just x = generateCaptcha <$> mkRandom20 "--------------------"
            Just y = generateCaptcha <$> mkRandom20 "---------------+----"
        snd x `shouldNotBe` snd y

{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE QuasiQuotes                              #-}
{-# LANGUAGE ScopedTypeVariables                      #-}

module Thentos.Sybil.CaptchaSpec where

import Control.Monad.IO.Class
import Data.Aeson (decode, FromJSON)
import Data.Pool (Pool)
import Data.String.Conversions (SBS, cs)
import Database.PostgreSQL.Simple (Connection, Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics (Generic)
import LIO (canFlowTo, lub, glb)
import LIO.DCLabel (DCLabel, (%%), (/\), (\/), toCNF)
import Test.Hspec.QuickCheck (modifyMaxSize)
import Test.Hspec (Spec, SpecWith, before, context, describe, it, shouldBe, shouldNotBe)
import Test.QuickCheck (property)

import qualified Data.ByteString as SBS

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

    it "writes pngs" $ do
        let Just x = generateCaptcha <$> mkRandom20 "-------------------8"
            binary :: SBS = fromImageData $ fst x
        liftIO $ SBS.writeFile "/tmp/captcha.png" binary
        SBS.length binary `shouldNotBe` 0

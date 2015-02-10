{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Arbitrary () where

import Control.Applicative ((<$>))
import Test.QuickCheck (Arbitrary(..))

import qualified Data.ByteString as SBS

import Types (EncryptedPass)

import Test.Util

instance Arbitrary EncryptedPass where
    arbitrary = encryptTestPassword . SBS.pack <$> arbitrary

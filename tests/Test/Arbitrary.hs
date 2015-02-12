{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Arbitrary () where

import Control.Applicative ((<$>))
import Test.QuickCheck (Arbitrary(..))

import qualified Data.ByteString as SBS

import Types (HashedSecret)

import Test.Util

instance Arbitrary (HashedSecret a) where
    arbitrary = encryptTestSecret . SBS.pack <$> arbitrary

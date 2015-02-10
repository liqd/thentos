{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Arbitrary () where

import Types (EncryptedPass)

import Test.Util

import Control.Applicative ((<$>))
import Test.QuickCheck (Arbitrary(..))

import qualified Data.ByteString as B

instance Arbitrary EncryptedPass where
    arbitrary = encryptTestPassword . B.pack <$> arbitrary

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.Arbitrary () where

import Control.Applicative ((<$>), (<*>))
import Data.String.Conversions (cs)
import LIO.DCLabel (DCLabel(DCLabel), (%%), (/\), (\/), CNF, toCNF)
import Test.QuickCheck (Arbitrary(..), sized, vectorOf, elements, Gen)

import qualified Data.ByteString as SBS

import Thentos.Types
-- import Thentos.Backend.Api.Adhocracy3

import Test.Util


instance Arbitrary (HashedSecret a) where
    arbitrary = encryptTestSecret . SBS.pack <$> arbitrary

instance Arbitrary DCLabel where
    arbitrary = DCLabel <$> arbitrary <*> arbitrary
    shrink (DCLabel s i) = [s %% False, s %% True, False %% i, True %% i]

instance Arbitrary CNF where
    arbitrary = sized $ \ l -> vectorOf l (elements readableStrings) >>= combine
      where
        combine :: [String] -> Gen CNF
        combine []     = toCNF <$> (arbitrary :: Gen Bool)
        combine (p:ps) = do
            o   <- arbitrary
            ps' <- combine ps
            let op = if o then (/\) else (\/)
            return $ p `op` ps'

-- | 25 most common adjectives according to the Oxford English
-- Dictionary.
readableStrings :: [String]
readableStrings =
    "good" : "new" : "first" : "last" : "long" : "great" : "little" :
    "own" : "other" : "old" : "right" : "big" : "high" : "different" :
    "small" : "large" : "next" : "early" : "young" : "important" :
    "few" : "public" : "bad" : "same" : "able" :
    []

instance Arbitrary UserFormData where
    arbitrary = UserFormData <$> s UserName <*> s UserPass <*> s UserEmail
      where s cons = cons . cs <$> elements readableStrings

-- | 'UserPass' has no 'Show' instance so we cannot accidentally leak
-- it into, say, a log file.  For testing, password leakage is not a
-- problem, but it helps using quickcheck, so we add orphan instances
-- here.
deriving instance Show UserPass
deriving instance Show UserFormData
-- deriving instance Show A3UserNoPass

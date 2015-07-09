{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

module Thentos.Test.Arbitrary () where

import Control.Applicative ((<$>), (<*>))
import Data.String.Conversions (cs)
import LIO.DCLabel (DCLabel(DCLabel), (%%), (/\), (\/), CNF, toCNF)
import Test.QuickCheck (Arbitrary(..), sized, vectorOf, elements, Gen)

import qualified Data.ByteString as SBS

import Thentos.Types
-- import Thentos.Backend.Api.Adhocracy3

import Thentos.Test.Config
import Thentos.Test.Core


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

-- | We just use one of the 'readableStrings' as name.
instance Arbitrary UserName where
    arbitrary = UserName . cs <$> elements readableStrings

instance Arbitrary UserEmail where
    arbitrary = do
        localName  <- elements readableStrings
        domainName <- elements readableStrings
        tld        <- elements topLevelDomains
        return . forceUserEmail . cs . concat $ [localName, "@", domainName, ".", tld]

-- | Some frequently used top-level domains.
topLevelDomains :: [String]
topLevelDomains = ["com", "net", "org", "info", "de", "fr", "ru", "co.uk"]

-- | Password made up of 10 to 20 random ASCII letters and numbers.
instance Arbitrary UserPass where
    arbitrary = do
        len  <- elements [10..20]
        pass <- vectorOf len $ elements passwordChar
        return . UserPass . cs $ pass
      where
        passwordChar = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

instance Arbitrary UserFormData where
    arbitrary = UserFormData <$> arbitrary <*> arbitrary <*> arbitrary

-- | 'UserPass' has no 'Show' instance so we cannot accidentally leak
-- it into, say, a log file.  For testing, password leakage is not a
-- problem, but it helps using quickcheck, so we add orphan instances
-- here.
deriving instance Show UserPass
deriving instance Show UserFormData
-- deriving instance Show A3UserNoPass

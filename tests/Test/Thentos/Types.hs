{-# LANGUAGE ScopedTypeVariables                      #-}

module Test.Thentos.Types where

import Types
import Test.Arbitrary ()

import Data.SafeCopy (safeGet, safePut)
import Data.Serialize.Get (runGet)
import Data.Serialize.Put (runPut)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (property)

tests :: Spec
tests = do
    describe "Types" $ do
        describe "EncrypedPass's safecopy instance" $
            it "is reversible" $ property $
                \ (pw :: EncryptedPass) ->
                    runGet safeGet (runPut $ safePut pw) == Right pw

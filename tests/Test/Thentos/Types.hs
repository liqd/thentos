{-# LANGUAGE ScopedTypeVariables                      #-}

module Test.Thentos.Types where

import Data.SafeCopy (safeGet, safePut)
import Data.Serialize.Get (runGet)
import Data.Serialize.Put (runPut)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (property)

import Types

import Test.Arbitrary ()


tests :: Spec
tests = do
    describe "Types" $ do
        describe "instance SafeCopy (HashedSecret a)" $
            it "is invertible" $ property $
                \ (pw :: HashedSecret a) ->
                    runGet safeGet (runPut $ safePut pw) == Right pw

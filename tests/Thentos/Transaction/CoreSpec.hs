{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}

module Thentos.Transaction.CoreSpec where

import Control.Lens ((%~), (%%~))
import Data.Functor.Infix ((<$>))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Test.Hspec (Spec, describe, it, shouldBe, hspec)

import Thentos.Types

import Test.Arbitrary ()


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "Thentos.Transaction.Core" $ do
    spec_polyQU


data CustomDB = CustomDB DB Int
  deriving (Eq, Show, Typeable, Generic)

instance AsDB CustomDB where
    asDB :: forall (f :: * -> *). Functor f => (DB -> f DB) -> CustomDB -> f CustomDB
    asDB f (CustomDB db i) = (`CustomDB` i) <$> f db

    asDBThentosError :: ThentosError DB -> ThentosError CustomDB
    asDBThentosError = CustomDBError

data instance (ThentosError CustomDB) = CustomDBError { fromCustomDBError :: ThentosError DB }

spec_polyQU :: Spec
spec_polyQU = describe "asDB, polyQuery, polyUpdate" $ do
    it "works" $ do
        let f :: DB -> DB
            f = id

            g :: AsDB db => db -> db
            g = asDB %~ f

            h :: DB -> (String, DB)
            h db = (show db, db)

            i :: AsDB db => db -> (String, db)
            i = asDB %%~ h

            test0 :: (Eq db, Show db, AsDB db) => db -> IO ()
            test0 db = do
                db `shouldBe` g db
                db `shouldBe` snd (i db)

        test0 $ emptyDB
        test0 $ CustomDB emptyDB 3

        let db = CustomDB emptyDB 3 in case i db of
              (prn', CustomDB db' x) -> do
                  prn' `shouldBe` show db'
                  db' `shouldBe` emptyDB
                  x `shouldBe` 3

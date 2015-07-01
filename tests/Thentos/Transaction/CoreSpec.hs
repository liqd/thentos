{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Thentos.Transaction.CoreSpec where

import Control.Lens ((%~), (%%~))
import Data.Functor.Infix ((<$>))
import Data.SafeCopy (SafeCopy(..), deriveSafeCopy, base)
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

instance CustomDB `Extends` DB where
    focus f (CustomDB db i) = (`CustomDB` i) <$> f db

    asDBThentosError :: ThentosError DB -> ThentosError CustomDB
    asDBThentosError = CustomDBError

data instance (ThentosError CustomDB) = CustomDBError { fromCustomDBError :: ThentosError DB }

deriving instance Show (ThentosError CustomDB)
deriving instance Read (ThentosError CustomDB)

instance SafeCopy (ThentosError CustomDB)
  where
    putCopy = putCopyViaShowRead
    getCopy = getCopyViaShowRead

spec_polyQU :: Spec
spec_polyQU = describe "asDB, polyQuery, polyUpdate" $ do
    it "works" $ do
        let f :: DB -> DB
            f = id

            g :: (db `Extends` DB) => db -> db
            g = focus %~ f

            h :: DB -> (String, DB)
            h db = (show db, db)

            i :: (db `Extends` DB) => db -> (String, db)
            i = focus %%~ h

            test0 :: (Eq db, Show db, db `Extends` DB) => db -> IO ()
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

$(deriveSafeCopy 0 'base ''CustomDB)

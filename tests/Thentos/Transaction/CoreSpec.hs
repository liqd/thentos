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
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Thentos.Transaction.CoreSpec where

import Control.Lens ((%~), (%%~))
import Data.Functor.Infix ((<$>))
import Data.SafeCopy (SafeCopy(..), deriveSafeCopy, base)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Test.Hspec (Spec, describe, it, shouldBe, hspec)

import Thentos.Types
--import Thentos.Transaction hiding (dbEvents) -- (transaction_names, AllUserIds(..))
import Thentos.Transaction.Core
import Thentos.Transaction.Types

import Data.Acid (openLocalState)
import Data.Acid.Advanced (query')

import Test.Arbitrary ()


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "Thentos.Transaction.Core" $ do
    spec_polyQU
    spec_useCustomDB

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

spec_useCustomDB :: Spec
spec_useCustomDB = describe "custom db" $ do
    it "works" $ \ () -> do
        st <- openLocalState (CustomDB emptyDB 3)
        u <- query' st $ AllUserIds
        u `shouldBe` Right []
        return ()

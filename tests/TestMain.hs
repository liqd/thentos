{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE NoImplicitPrelude                        #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}

{-# OPTIONS -fwarn-unused-imports -fwarn-incomplete-patterns -fdefer-type-errors #-}

module TestMain
where

import Control.Lens
import Control.Monad
import Data.Acid
import Data.Maybe
import Data.Thyme
import Filesystem
import GHC.Exts
import Prelude
import Test.Hspec

import DB
import Types


main :: IO ()
main = hspec $ do
  describe "DB" . before setupDB . after teardownDB $ do
    describe "InsertUser, LookupUser, DeleteUser" $ do
      it "works" . withDB $ \ st -> do
        update st $ InsertUser user1
        u <- query st $ LookupUser 1
        (u >>= (^. userID)) `shouldBe` Just 1
        update st $ DeleteUser 1
        u <- query st $ LookupUser 1
        u `shouldBe` Nothing

    describe "StartSession" $ do
      it "works" . withDB $ \ st -> do
        update st $ DeleteUser 1  -- FIXME: ?  why does user1 exist here?
        from <- getCurrentTime
        to <- getCurrentTime
        update st (StartSession 1 from to) `shouldThrow` anyException
        update st $ InsertUser user1
        void $ update st (StartSession 1 from to)


dbPath :: FilePath
dbPath = ".test-db/"

user1, user2, user3 :: User
user1 = User (Just 1) "name1" "passwd" "em@il" [] Nothing
user2 = User (Just 2) "name2" "passwd" "em@il" ["group1", "group2"] Nothing
user3 = User (Just 3) "name3" "3" "3" ["23"] Nothing

withDB :: (AcidState DB -> IO ()) -> IO ()
withDB prog = do
  st <- openLocalStateFrom dbPath emptyDB
  prog st
  closeAcidState st

setupDB :: IO ()
setupDB = withDB $ \ st -> do
    update_ st $ InsertUser user1
    update_ st $ InsertUser user2

teardownDB :: () -> IO ()
teardownDB _ = removeTree $ fromString dbPath

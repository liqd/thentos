{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}

{-# OPTIONS -fwarn-unused-imports -fwarn-incomplete-patterns -fdefer-type-errors #-}

module TestMain
where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Monad (void)
import Data.Data (Proxy(Proxy))
import Data.Acid (AcidState, openLocalStateFrom, closeAcidState, query, update)
import Data.Thyme (getCurrentTime)
import Filesystem (removeTree)
import GHC.Exts (fromString)
import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)
import Test.Hspec (hspec, describe, it, before, after, shouldBe, shouldThrow, anyException)

import Api
import DB
import Types


data Config =
    Config
      { dbPath :: FilePath
      , restPort :: Int
      }
  deriving (Eq, Show)

config :: Config
config =
    Config
      { dbPath = ".test-db/"
      , restPort = 8002
      }


main :: IO ()
main = hspec $ do
  describe "DB" . before setupDB . after teardownDB $ do
    describe "AddUser, LookupUser, DeleteUser" $ do
      it "works" . withDB $ \ st -> do
        uid <- update st $ AddUser user1
        Just user1' <- query st $ LookupUser uid
        user1' `shouldBe` user1
        update st $ DeleteUser 1
        u <- query st $ LookupUser 1
        u `shouldBe` Nothing

      it "hspec meta: `setupDB, teardownDB` arecalled once for every `it` here." . withDB $ \ st -> do
        uids <- query st AllUserIDs
        uids `shouldBe` [0, 1]

    describe "StartSession" $ do
      it "works" . withDB $ \ st -> do
        from <- getCurrentTime
        to <- getCurrentTime
        update st (StartSession 0 "nosuchservice" from to) `shouldThrow` anyException
        sid :: ServiceId <- update st $ AddService
        void $ update st (StartSession 0 sid from to)

  describe "Api" . before setupApi . after teardownApi $ do
    describe "/user/" $ do
      it "works" $ \ _ -> do
        True `shouldBe` True


user1, user2, user3 :: User
user1 = User "name1" "passwd" "em@il" [] Nothing
user2 = User "name2" "passwd" "em@il" ["group1", "group2"] Nothing
user3 = User "name3" "3" "3" ["23"] Nothing

withDB :: (AcidState DB -> IO ()) -> IO ()
withDB prog = do
  st <- openLocalStateFrom (dbPath config) emptyDB
  prog st
  closeAcidState st

setupDB :: IO ()
setupDB = withDB $ \ st -> do
    update_ st $ AddUser user1
    update_ st $ AddUser user2

teardownDB :: () -> IO ()
teardownDB _ = removeTree $ fromString (dbPath config)

setupApi :: IO (Async ())
setupApi = async . withDB $ \ st -> do
    run (restPort config) $ serve (Proxy :: Proxy App) (app st)

teardownApi :: (Async ()) -> IO ()
teardownApi = cancel

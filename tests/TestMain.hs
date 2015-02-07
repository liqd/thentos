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
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS  #-}

module TestMain
where

import Control.Monad (void)
import Data.Acid.Advanced (query', update')
import Data.Either (isLeft, isRight)
import Data.Functor.Infix ((<$>))
import Data.Thyme (getCurrentTime)
import LIO (canFlowTo)
import LIO.DCLabel ((%%), (/\), (\/), toCNF)
import Test.Hspec (Spec, hspec, describe, it, before, after, shouldBe, shouldSatisfy)

import DB
import Api
import Types

import Test.Util
import qualified Test.Thentos.Backend.Api.Simple


-- * test suite

main :: IO ()
main = hspec $ do
  Test.Thentos.Backend.Api.Simple.tests
  adhoc


adhoc :: Spec
adhoc = do
  describe "DB" . before setupDB . after teardownDB $ do
    describe "hspec meta" $ do
      it "`setupDB, teardownDB` are called once for every `it` here (part I)." $ \ (st, _, _) -> do
        Right _ <- update' st $ AddUser user3 allowEverything
        True `shouldBe` True

      it "`setupDB, teardownDB` are called once for every `it` here (part II)." $ \ (st, _, _) -> do
        uids <- query' st $ AllUserIds allowEverything
        uids `shouldBe` Right [UserId 0, UserId 1, UserId 2]  -- (no (UserId 2))

    describe "AddUser, LookupUser, DeleteUser" $ do
      it "works" $ \ (st, _, _) -> do
        Right uid <- update' st $ AddUser user3 allowEverything
        Right (uid', user3') <- query' st $ LookupUser uid allowEverything
        user3' `shouldBe` user3
        uid' `shouldBe` uid
        void . update' st $ DeleteUser uid allowEverything
        u <- query' st $ LookupUser uid allowEverything
        u `shouldBe` Left NoSuchUser

      it "guarantee that email addresses are unique" $ \ (st, _, _) -> do
        result <- update' st $ AddUser user1 allowEverything
        result `shouldBe` Left UserEmailAlreadyExists

    describe "DeleteUser" $ do
      it "user can delete herself, even if not admin" $ \ (st, _, _) -> do
        let uid = UserId 1
        result <- update' st $ DeleteUser uid (UserA uid *%% UserA uid)
        result `shouldBe` Right ()

      it "nobody else but the deleted user and admin can do this" $ \ (st, _, _) -> do
        result <- update' st $ DeleteUser (UserId 1) (UserA (UserId 2) *%% UserA (UserId 2))
        result `shouldSatisfy` isLeft

    describe "UpdateUser" $ do
      it "changes user if it exists" $ \ (st, _, _) -> do
        result <- update' st $ UpdateUser (UserId 1) user1 allowEverything
        result `shouldBe` Right ()
        result2 <- query' st $ LookupUser (UserId 1) allowEverything
        result2 `shouldBe` (Right (UserId 1, user1))

      it "throws an error if user does not exist" $ \ (st, _, _) -> do
        result <- update' st $ UpdateUser (UserId 391) user3 allowEverything
        result `shouldBe` Left NoSuchUser

    describe "AddUsers" $ do
      it "works" $ \ (st, _, _) -> do
        result <- update' st $ AddUsers [user3, user4, user5] allowEverything
        result `shouldBe` Right (map UserId [3, 4, 5])

      it "rolls back in case of error (adds all or nothing)" $ \ (st, _, _) -> do
        Left UserEmailAlreadyExists <- update' st $ AddUsers [user4, user3, user3] allowEverything
        result <- query' st $ AllUserIds allowEverything
        result `shouldBe` Right (map UserId [0, 1, 2])

    describe "AddService, LookupService, DeleteService" $ do
      it "works" $ \ asg@(st, _, _) -> do
        Right (service1_id, _s1_key) <- runAction' (asg, allowEverything) addService
        Right (service2_id, _s2_key) <- runAction' (asg, allowEverything) addService
        Right service1 <- query' st $ LookupService service1_id allowEverything
        Right service2 <- query' st $ LookupService service2_id allowEverything
        service1 `shouldBe` service1 -- sanity check for reflexivity of Eq
        service1 `shouldSatisfy` (/= service2) -- should have different keys
        void . update' st $ DeleteService service1_id allowEverything
        Left NoSuchService <- query' st $ LookupService service1_id allowEverything
        return ()

    describe "StartSession" $ do
      it "works" $ \ asg -> do
        from <- TimeStamp <$> getCurrentTime
        let timeout = Timeout 600
        Left NoSuchService <- runAction' (asg, allowEverything) $ startSession (UserId 0) "NoSuchService" from timeout
        Right (sid :: ServiceId, _) <- runAction' (asg, allowEverything) $ addService
        Right _ <- runAction' (asg, allowEverything) $ startSession (UserId 0) sid from timeout
        return ()

    describe "agents and roles" $ do
      describe "assign" $ do
        it "can be called by admins" $ \ (st, _, _) -> do
          let targetAgent = UserA $ UserId 1
          result <- update' st $ AssignRole targetAgent RoleAdmin (RoleAdmin *%% RoleAdmin)
          result `shouldSatisfy` isRight

        it "can NOT be called by any non-admin agents" $ \ (st, _, _) -> do
          let targetAgent = UserA $ UserId 1
          result <- update' st $ AssignRole targetAgent RoleAdmin (targetAgent *%% targetAgent)
          result `shouldSatisfy` isLeft

      describe "lookup" $ do
        it "can be called by admins" $ \ (st, _, _) -> do
          let targetAgent = UserA $ UserId 1
          result :: Either DbError [Role] <- query' st $ LookupAgentRoles targetAgent (RoleAdmin *%% RoleAdmin)
          result `shouldSatisfy` isRight

        it "can be called by user for her own roles" $ \ (st, _, _) -> do
          let targetAgent = UserA $ UserId 1
          result <- query' st $ LookupAgentRoles targetAgent (targetAgent *%% targetAgent)
          result `shouldSatisfy` isRight

        it "can NOT be called by other users" $ \ (st, _, _) -> do
          let targetAgent = UserA $ UserId 1
              askingAgent = UserA $ UserId 2
          result <- query' st $ LookupAgentRoles targetAgent (askingAgent *%% askingAgent)
          result `shouldSatisfy` isLeft

  -- This test doesn't really test thentos code, but it helps
  -- understanding DCLabel.
  describe "DCLabel" $
    it "works" $ do
      let a = toCNF ("a" :: String)
          b = toCNF ("b" :: String)
          c = toCNF ("c" :: String)

      and [ b \/ a %% a `canFlowTo` a %% a
          , a %% a /\ b `canFlowTo` a %% a
          , a %% (a /\ b) \/ (a /\ c) `canFlowTo` a %% a
          , not $ a %% (a /\ b) \/ (a /\ c) `canFlowTo` a %% b
          ,       True  %% False `canFlowTo` True %% False
          ,       True  %% False `canFlowTo` False %% True
          ,       True  %% True  `canFlowTo` False %% True
          ,       False %% False `canFlowTo` False %% True
          , not $ False %% True  `canFlowTo` True  %% False
          , not $ True  %% True  `canFlowTo` True  %% False
          , not $ False %% False `canFlowTo` True  %% False
          , True
          ] `shouldBe` True

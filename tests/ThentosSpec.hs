{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module ThentosSpec where

import Control.Lens ((.~))
import Control.Monad (void)
import Data.Acid (AcidState)
import Data.Acid.Advanced (query', update')
import Data.Either (isLeft, isRight)
import Test.Hspec (Spec, hspec, describe, it, before, after, shouldBe, shouldSatisfy)

import Thentos.Action
import Thentos.Action.Core
import Thentos.Types

import qualified Thentos.Transaction as T

import Test.Config
import Test.Util


tests :: IO ()
tests = hspec spec

getStateDB :: DBTS -> AcidState DB
getStateDB (DBTS _ (ActionState (st, _, _))) = st

getStateAS :: DBTS -> ActionState DB
getStateAS (DBTS _ as) = as

spec :: Spec
spec = describe "DB" . before setupDB . after teardownDB $ do
    describe "hspec meta" $ do
        it "`setupDB, teardownDB` are called once for every `it` here (part I)." $ \ (getStateDB -> st) -> do
            Right _ <- update' st $ T.AddUser user3
            True `shouldBe` True

        it "`setupDB, teardownDB` are called once for every `it` here (part II)." $ \ (getStateDB -> st) -> do
            uids <- query' st $ T.AllUserIds
            uids `shouldBe` Right [UserId 0, UserId 1, UserId 2]  -- (no (UserId 2))

    describe "checkPassword" $ do
        it "..." $ \ (getStateAS -> asg) -> do
            byId <- runActionE asg $ startThentosSessionByUserId (UserId 0) (UserPass "god")
            byId `shouldSatisfy` isRight
            byName <- runActionE asg $ startThentosSessionByUserName (UserName "god") (UserPass "god")
            byName `shouldSatisfy` isRight

    describe "AddUser, LookupUser, DeleteUser" $ do
        it "works" $ \ (getStateDB -> st) -> do
            Right uid <- update' st $ T.AddUser user3
            Right (uid', user3') <- query' st $ T.LookupUser uid
            user3' `shouldBe` user3
            uid' `shouldBe` uid
            void . update' st $ T.DeleteUser uid
            u <- query' st $ T.LookupUser uid
            u `shouldBe` Left NoSuchUser

        it "guarantee that user names are unique" $ \ (getStateDB -> st) -> do
            result <- update' st $ T.AddUser (userEmail .~ (UserEmail "new@one.com") $ user1)
            result `shouldBe` Left UserNameAlreadyExists

        it "guarantee that user email addresses are unique" $ \ (getStateDB -> st) -> do
            result <- update' st $ T.AddUser (userName .~ (UserName "newone") $ user1)
            result `shouldBe` Left UserEmailAlreadyExists

    describe "DeleteUser" $ do
        it "user can delete herself, even if not admin" $ \ (getStateAS -> asg) -> do
            let uid = UserId 1
            result <- runActionWithPrivsE [UserA uid] asg $ deleteUser uid
            result `shouldSatisfy` isRight

        it "nobody else but the deleted user and admin can do this" $ \ (getStateAS -> asg) -> do
            result <- runActionWithPrivsE [UserA (UserId 2)] asg $ deleteUser (UserId 1)
            result `shouldSatisfy` isLeft

    describe "UpdateUser" $ do
        it "changes user if it exists" $ \ (getStateDB -> st) -> do
            result <- update' st $ T.UpdateUserField (UserId 1)
                                                   (T.UpdateUserFieldName "fka_user1")
            result `shouldBe` Right ()
            result2 <- query' st $ T.LookupUser (UserId 1)
            result2 `shouldBe` (Right (UserId 1, userName .~ "fka_user1" $ user1))
        it "throws an error if user does not exist" $ \ (getStateDB -> st) -> do
            result <- update' st $ T.UpdateUserField (UserId 391)
                                                   (T.UpdateUserFieldName "moo")
            result `shouldBe` Left NoSuchUser

    describe "AddUsers" $ do
        it "works" $ \ (getStateDB -> st) -> do
            result <- update' st $ T.AddUsers [user3, user4, user5]
            result `shouldBe` Right (map UserId [3, 4, 5])

        it "rolls back in case of error (adds all or nothing)" $ \ (getStateDB -> st) -> do
            Left UserNameAlreadyExists <- update' st $ T.AddUsers [user4, user3, user3]
            result <- query' st $ T.AllUserIds
            result `shouldBe` Right (map UserId [0, 1, 2])

    describe "AddService, LookupService, DeleteService" $ do
        it "works" $ \ (getStateAS -> asg@(ActionState (st, _, _))) -> do
            let addsvc name desc = runActionE asg $ addService (UserA (UserId 0)) name desc
            Right (service1_id, _s1_key) <- addsvc "fake name" "fake description"
            Right (service2_id, _s2_key) <- addsvc "different name" "different description"
            Right service1 <- query' st $ T.LookupService service1_id
            Right service2 <- query' st $ T.LookupService service2_id
            service1 `shouldBe` service1 -- sanity check for reflexivity of Eq
            service1 `shouldSatisfy` (/= service2) -- should have different keys
            void . update' st $ T.DeleteService service1_id
            Left NoSuchService <- query' st $ T.LookupService service1_id
            return ()

    describe "StartSession" $ do
        it "works" $ \ (getStateAS -> asg) -> do
            result <- runActionE asg $ startThentosSessionByAgent (UserA $ UserId 0)
            result `shouldSatisfy` isRight
            return ()

    describe "agents and roles" $ do
        describe "assign" $ do
            it "can be called by admins" $ \ (getStateAS -> asg) -> do
                let targetAgent = UserA $ UserId 1
                result <- runActionWithPrivsE [RoleAdmin] asg $ assignRole targetAgent (RoleBasic RoleAdmin)
                result `shouldSatisfy` isRight

            it "can NOT be called by any non-admin agents" $ \ (getStateAS -> asg) -> do
                let targetAgent = UserA $ UserId 1
                result <- runActionWithPrivsE [targetAgent] asg $ assignRole targetAgent (RoleBasic RoleAdmin)
                result `shouldSatisfy` isLeft

        describe "lookup" $ do
            it "can be called by admins" $ \ (getStateAS -> asg) -> do
                let targetAgent = UserA $ UserId 1
                result <- runActionWithPrivsE [RoleAdmin] asg $ agentRoles targetAgent
                result `shouldSatisfy` isRight

            it "can be called by user for her own roles" $ \ (getStateAS -> asg) -> do
                let targetAgent = UserA $ UserId 1
                result <- runActionWithPrivsE [targetAgent] asg $ agentRoles targetAgent
                result `shouldSatisfy` isRight

            it "can NOT be called by other users" $ \ (getStateAS -> asg) -> do
                let targetAgent = UserA $ UserId 1
                    askingAgent = UserA $ UserId 2
                result <- runActionWithPrivsE [askingAgent] asg $ agentRoles targetAgent
                result `shouldSatisfy` isLeft

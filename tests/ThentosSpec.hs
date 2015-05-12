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
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Data.Acid.Advanced (query', update')
import Data.Either (isLeft, isRight)
import LIO.DCLabel (dcPublic, (%%))
import Test.Hspec (Spec, hspec, describe, it, before, after, shouldBe, shouldSatisfy, pendingWith)

import Thentos.Action
import Thentos.Action.Core
import Thentos.Transaction hiding (addService)
import Thentos.Types

import Test.Config
import Test.Util


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "DB" . before (setupDB testThentosConfig) . after teardownDB $ do
    describe "hspec meta" $ do
        it "`setupDB, teardownDB` are called once for every `it` here (part I)." $ \ (ActionState (st, _, _)) -> do
            Right _ <- update' st $ AddUser user3
            True `shouldBe` True

        it "`setupDB, teardownDB` are called once for every `it` here (part II)." $ \ (ActionState (st, _, _)) -> do
            uids <- query' st $ AllUserIds
            uids `shouldBe` Right [UserId 0, UserId 1, UserId 2]  -- (no (UserId 2))

    describe "checkPassword" $ do
        it "..." $ \ (asg :: ActionState) -> do
            byId <- runActionE dcPublic asg $ startThentosSessionByUserId (UserId 0) (UserPass "god")
            byId `shouldSatisfy` isRight
            byName <- runActionE dcPublic asg $ startThentosSessionByUserName (UserName "god") (UserPass "god")
            byName `shouldSatisfy` isRight

    describe "AddUser, LookupUser, DeleteUser" $ do
        it "works" $ \ (ActionState (st, _, _)) -> do
            Right uid <- update' st $ AddUser user3
            Right (uid', user3') <- query' st $ LookupUser uid
            user3' `shouldBe` user3
            uid' `shouldBe` uid
            void . update' st $ DeleteUser uid
            u <- query' st $ LookupUser uid
            u `shouldBe` Left NoSuchUser

        it "guarantee that user names are unique" $ \ (ActionState (st, _, _)) -> do
            result <- update' st $ AddUser (userEmail .~ (UserEmail "new@one.com") $ user1)
            result `shouldBe` Left UserNameAlreadyExists

        it "guarantee that user email addresses are unique" $ \ (ActionState (st, _, _)) -> do
            result <- update' st $ AddUser (userName .~ (UserName "newone") $ user1)
            result `shouldBe` Left UserEmailAlreadyExists

    describe "DeleteUser" $ do
        it "user can delete herself, even if not admin" $ \ asg -> do
            let uid = UserId 1
            result <- runActionE (UserA uid %% UserA uid) asg . update'P $ DeleteUser uid
            result `shouldSatisfy` isRight

        it "nobody else but the deleted user and admin can do this" $ \ asg -> do

            liftIO $ pendingWith "permissions are out of order!"  -- FIXME

            result <- runActionE (UserA (UserId 2) %% UserA (UserId 2)) asg . update'P $ DeleteUser (UserId 1)
            result `shouldSatisfy` isLeft

    describe "UpdateUser" $ do
        it "changes user if it exists" $ \ (ActionState (st, _, _)) -> do
            result <- update' st $ UpdateUserField (UserId 1)
                                                   (UpdateUserFieldName "fka_user1")
            result `shouldBe` Right ()
            result2 <- query' st $ LookupUser (UserId 1)
            result2 `shouldBe` (Right (UserId 1, userName .~ "fka_user1" $ user1))
        it "throws an error if user does not exist" $ \ (ActionState (st, _, _)) -> do
            result <- update' st $ UpdateUserField (UserId 391)
                                                   (UpdateUserFieldName "moo")
            result `shouldBe` Left NoSuchUser

    describe "AddUsers" $ do
        it "works" $ \ (ActionState (st, _, _)) -> do
            result <- update' st $ AddUsers [user3, user4, user5]
            result `shouldBe` Right (map UserId [3, 4, 5])

        it "rolls back in case of error (adds all or nothing)" $ \ (ActionState (st, _, _)) -> do
            Left UserNameAlreadyExists <- update' st $ AddUsers [user4, user3, user3]
            result <- query' st $ AllUserIds
            result `shouldBe` Right (map UserId [0, 1, 2])

    describe "AddService, LookupService, DeleteService" $ do
        it "works" $ \ asg@(ActionState (st, _, _)) -> do
            let addsvc name desc = runActionE dcPublic asg $ addService (UserA (UserId 0)) name desc
            Right (service1_id, _s1_key) <- addsvc "fake name" "fake description"
            Right (service2_id, _s2_key) <- addsvc "different name" "different description"
            Right service1 <- query' st $ LookupService service1_id
            Right service2 <- query' st $ LookupService service2_id
            service1 `shouldBe` service1 -- sanity check for reflexivity of Eq
            service1 `shouldSatisfy` (/= service2) -- should have different keys
            void . update' st $ DeleteService service1_id
            Left NoSuchService <- query' st $ LookupService service1_id
            return ()

    describe "StartSession" $ do
        it "works" $ \ asg -> do
            result <- runActionE dcPublic asg $ startThentosSessionByAgent (UserA $ UserId 0)
            result `shouldSatisfy` isRight
            return ()

    describe "agents and roles" $ do
        describe "assign" $ do
            it "can be called by admins" $ \ asg -> do
                let targetAgent = UserA $ UserId 1
                result <- runActionE (RoleAdmin %% RoleAdmin) asg . update'P $ AssignRole targetAgent (RoleBasic RoleAdmin)
                result `shouldSatisfy` isRight

            it "can NOT be called by any non-admin agents" $ \ asg -> do

                liftIO $ pendingWith "permissions are out of order!"  -- FIXME

                let targetAgent = UserA $ UserId 1
                result <- runActionE (targetAgent %% targetAgent) asg . update'P $ AssignRole targetAgent (RoleBasic RoleAdmin)
                result `shouldSatisfy` isLeft

        describe "lookup" $ do
            it "can be called by admins" $ \ asg -> do
                let targetAgent = UserA $ UserId 1
                result <- runActionE (RoleAdmin %% RoleAdmin) asg . query'P $ AgentRoles targetAgent
                result `shouldSatisfy` isRight

            it "can be called by user for her own roles" $ \ asg -> do
                let targetAgent = UserA $ UserId 1
                result <- runActionE (targetAgent %% targetAgent) asg . query'P $ AgentRoles targetAgent
                result `shouldSatisfy` isRight

            it "can NOT be called by other users" $ \ asg -> do

                liftIO $ pendingWith "permissions are out of order!"  -- FIXME

                let targetAgent = UserA $ UserId 1
                    askingAgent = UserA $ UserId 2
                result <- runActionE (askingAgent %% askingAgent) asg . query'P $ AgentRoles targetAgent
                result `shouldSatisfy` isLeft

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

module ThentosSpec where

import Control.Lens ((.~))
import Control.Monad (void)
import Control.Concurrent (MVar)
import Crypto.Random (SystemRNG)
import Data.Acid.Advanced (query', update')
import Data.Either (isLeft, isRight)
import Test.Hspec (Spec, hspec, describe, it, before, after, shouldBe, shouldSatisfy)

import Thentos.Api
import Thentos.DB
import Thentos.Types

import Test.Config
import Test.Util


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "DB" . before (setupDB testThentosConfig) . after teardownDB $ do
    describe "hspec meta" $ do
        it "`setupDB, teardownDB` are called once for every `it` here (part I)." $ \ (st, _, _) -> do
            Right _ <- update' st $ AddUser user3 allowEverything
            True `shouldBe` True

        it "`setupDB, teardownDB` are called once for every `it` here (part II)." $ \ (st, _, _) -> do
            uids <- query' st $ AllUserIds allowEverything
            uids `shouldBe` Right [UserId 0, UserId 1, UserId 2]  -- (no (UserId 2))

    describe "checkPassword" $ do
        it "..." $ \ (asg :: ActionStateGlobal (MVar SystemRNG)) -> do
            byId <- runAction' (asg, allowEverything) $ checkPasswordByUserId (UserId 0) (UserPass "god")
            byId `shouldSatisfy` isRight
            byName <- runAction' (asg, allowEverything) $ checkPasswordByUserName (UserName "god") (UserPass "god")
            byName `shouldSatisfy` isRight

    describe "AddUser, LookupUser, DeleteUser" $ do
        it "works" $ \ (st, _, _) -> do
            Right uid <- update' st $ AddUser user3 allowEverything
            Right (uid', user3') <- query' st $ LookupUser uid allowEverything
            user3' `shouldBe` user3
            uid' `shouldBe` uid
            void . update' st $ DeleteUser uid allowEverything
            u <- query' st $ LookupUser uid allowEverything
            u `shouldBe` Left NoSuchUser

        it "guarantee that user names are unique" $ \ (st, _, _) -> do
            result <- update' st $ AddUser (userEmail .~ (UserEmail "new@one.com") $ user1) allowEverything
            result `shouldBe` Left UserNameAlreadyExists

        it "guarantee that user email addresses are unique" $ \ (st, _, _) -> do
            result <- update' st $ AddUser (userName .~ (UserName "newone") $ user1) allowEverything
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
            result <- update' st $ UpdateUserField (UserId 1)
                                                   (UpdateUserFieldName "fka_user1")
                                                   allowEverything
            result `shouldBe` Right ()
            result2 <- query' st $ LookupUser (UserId 1) allowEverything
            result2 `shouldBe` (Right (UserId 1, userName .~ "fka_user1" $ user1))
        it "throws an error if user does not exist" $ \ (st, _, _) -> do
            result <- update' st $ UpdateUserField (UserId 391)
                                                   (UpdateUserFieldName "moo")
                                                   allowEverything
            result `shouldBe` Left NoSuchUser

    describe "AddUsers" $ do
        it "works" $ \ (st, _, _) -> do
            result <- update' st $ AddUsers [user3, user4, user5] allowEverything
            result `shouldBe` Right (map UserId [3, 4, 5])

        it "rolls back in case of error (adds all or nothing)" $ \ (st, _, _) -> do
            Left UserNameAlreadyExists <- update' st $ AddUsers [user4, user3, user3] allowEverything
            result <- query' st $ AllUserIds allowEverything
            result `shouldBe` Right (map UserId [0, 1, 2])

    describe "AddService, LookupService, DeleteService" $ do
        it "works" $ \ asg@(st, _, _) -> do
            Right (service1_id, _s1_key) <- runAction' (asg, allowEverything) $ addService "fake name" "fake description"
            Right (service2_id, _s2_key) <- runAction' (asg, allowEverything) $ addService "different name" "different description"
            Right service1 <- query' st $ LookupService service1_id allowEverything
            Right service2 <- query' st $ LookupService service2_id allowEverything
            service1 `shouldBe` service1 -- sanity check for reflexivity of Eq
            service1 `shouldSatisfy` (/= service2) -- should have different keys
            void . update' st $ DeleteService service1_id allowEverything
            Left NoSuchService <- query' st $ LookupService service1_id allowEverything
            return ()

    describe "StartSession" $ do
        it "works" $ \ asg -> do
            result <- runAction' (asg, allowEverything) $ startSessionNoPass (UserA $ UserId 0)
            result `shouldSatisfy` isRight
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
                result :: Either ThentosError [Role] <- query' st $ LookupAgentRoles targetAgent (RoleAdmin *%% RoleAdmin)
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

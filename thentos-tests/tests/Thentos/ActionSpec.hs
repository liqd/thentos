{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Thentos.ActionSpec where

import Control.Applicative ((<$>))
import Control.Concurrent (MVar, newMVar)
import Control.Lens ((.~), (^.))
import Control.Monad (void)
import Crypto.Random (ChaChaDRG, drgNew)
import Data.Acid.Memory (openMemoryState)
import Data.Either (isLeft, isRight)
import LIO.DCLabel ((%%))
import Test.Hspec (Spec, SpecWith, describe, it, before, after, shouldBe, shouldContain,
                   shouldNotContain, shouldSatisfy, hspec)

import Thentos.Test.Arbitrary ()
import Thentos.Test.Config
import Thentos.Test.Core
import Thentos.Test.CustomDB

import LIO.Missing
import Thentos.Action
import Thentos.Action.Core
import Thentos.Types


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = do
    describe "Thentos.Action" . before (thentosTestConfig >>= createActionState) $ do
        spec_user
        spec_service
        spec_agentsAndRoles
        spec_session
    spec_customDb


spec_user :: SpecWith (ActionState DB)
spec_user = describe "user" $ do
    describe "addUser, lookupUser, deleteUser" $ do
        it "works" $ \ sta -> do
            let user = testUsers !! 0
            uid <- runActionWithPrivs [RoleAdmin] sta $ addUser (head testUserForms)
            (uid', user') <- runActionWithPrivs [RoleAdmin] sta $ lookupUser uid
            uid' `shouldBe` uid
            user' `shouldBe` (userPassword .~ (user' ^. userPassword) $ user)
            void . runActionWithPrivs [RoleAdmin] sta $ deleteUser uid
            Left (ActionErrorThentos NoSuchUser) <-
                runActionWithClearanceE dcBottom sta $ lookupUser uid
            return ()

        it "guarantee that user names are unique" $ \ sta -> do
            (_, _, user) <- runActionWithClearance dcBottom sta $ addTestUser 1
            let userFormData = UserFormData (user ^. userName)
                                            (UserPass "foo")
                                            (forceUserEmail "new@one.com")
            Left (ActionErrorThentos e) <- runActionWithPrivsE [RoleAdmin] sta $
                addUser userFormData
            e `shouldBe` UserNameAlreadyExists

        it "guarantee that user email addresses are unique" $ \ sta -> do
            (_, _, user) <- runActionWithClearance dcBottom sta $ addTestUser 1
            let userFormData = UserFormData (UserName "newOne")
                                            (UserPass "foo")
                                            (user ^. userEmail)
            Left (ActionErrorThentos e) <- runActionWithPrivsE [RoleAdmin] sta $ addUser userFormData
            e `shouldBe` UserEmailAlreadyExists

    describe "addUsers" $ do
        it "works" $ \ sta -> do
            result <- runActionWithPrivs [RoleAdmin] sta $
                addUsers ((testUserForms !!) <$> [2..4])
            result `shouldBe` (UserId <$> [1..3])

        it "rolls back in case of error (adds all or nothing)" $ \ sta -> do
            _ <- runActionWithPrivs [RoleAdmin] sta $ addUser (testUserForms !! 4)
            Left (ActionErrorThentos e) <- runActionWithPrivsE [RoleAdmin] sta
                $ addUsers ((testUserForms !!) <$> [2..4])
            e `shouldBe` UserNameAlreadyExists
            result <- runActionWithPrivs [RoleAdmin] sta allUserIds
            result `shouldBe` (UserId <$> [0..1])

    describe "DeleteUser" $ do
        it "user can delete herself, even if not admin" $ \ sta -> do
            (uid, _, _) <- runActionWithClearance dcBottom sta $ addTestUser 3
            result <- runActionWithPrivsE [UserA uid] sta $ deleteUser uid
            result `shouldSatisfy` isRight

        it "nobody else but the deleted user and admin can do this" $ \ sta -> do
            (uid,  _, _) <- runActionWithClearance dcBottom sta $ addTestUser 3
            (uid', _, _) <- runActionWithClearance dcBottom sta $ addTestUser 4
            result <- runActionWithPrivsE [UserA uid] sta $ deleteUser uid'
            result `shouldSatisfy` isLeft

    describe "UpdateUser" $ do
        it "changes user if it exists" $ \ sta -> do
            (uid, _, user) <- runActionWithClearance dcBottom sta $ addTestUser 1
            runActionWithPrivs [UserA uid] sta $
                updateUserField uid (UpdateUserFieldName "fka_user1")

            result <- runActionWithPrivs [UserA uid] sta $ lookupUser uid
            result `shouldBe` (UserId 1, userName .~ "fka_user1" $ user)

        it "throws an error if user does not exist" $ \ sta -> do
            Left (ActionErrorThentos e) <- runActionWithPrivsE [RoleAdmin] sta $
                updateUserField (UserId 391) (UpdateUserFieldName "moo")
            e `shouldBe` NoSuchUser

    describe "checkPassword" $ do
        it "works" $ \ sta -> do
            void . runAction sta $ startThentosSessionByUserId godUid godPass
            void . runAction sta $ startThentosSessionByUserName godName godPass


spec_service :: SpecWith (ActionState DB)
spec_service = describe "service" $ do
    describe "addService, lookupService, deleteService" $ do
        it "works" $ \ sta -> do
            let addsvc name desc = runActionWithClearanceE (UserA godUid %% UserA godUid) sta
                    $ addService (UserA (UserId 0)) name desc
            Right (service1_id, _s1_key) <- addsvc "fake name" "fake description"
            Right (service2_id, _s2_key) <- addsvc "different name" "different description"
            service1 <- runActionWithPrivs [RoleAdmin] sta $ lookupService service1_id
            service2 <- runActionWithPrivs [RoleAdmin] sta $ lookupService service2_id
            service1 `shouldBe` service1 -- sanity check for reflexivity of Eq
            service1 `shouldSatisfy` (/= service2) -- should have different keys
            void . runActionWithPrivs [RoleAdmin] sta $ deleteService service1_id
            Left (ActionErrorThentos NoSuchService) <-
                runActionWithPrivsE [RoleAdmin] sta $ lookupService service1_id
            return ()

    describe "autocreateServiceIfMissing" $ do
        it "adds service if missing" $ \(DBTS _ sta) -> do
            let owner = UserA $ UserId 0
            sid <- runActionWithPrivs [RoleAdmin] sta $ freshServiceId
            allSids <- runActionWithPrivs [RoleAdmin] sta allServiceIds
            allSids `shouldNotContain` [sid]
            runActionWithPrivs [RoleAdmin] sta $ autocreateServiceIfMissing'P owner sid
            allSids' <- runActionWithPrivs [RoleAdmin] sta allServiceIds
            allSids' `shouldContain` [sid]

        it "does nothing if service exists" $ \(DBTS _ sta) -> do
            let owner = UserA $ UserId 0
            (sid, _) <- runActionWithPrivs [RoleAdmin] sta
                            $ addService owner "fake name" "fake description"
            allSids <- runActionWithPrivs [RoleAdmin] sta allServiceIds
            runActionWithPrivs [RoleAdmin] sta $ autocreateServiceIfMissing'P owner sid
            allSids' <- runActionWithPrivs [RoleAdmin] sta allServiceIds
            allSids `shouldBe` allSids'

spec_agentsAndRoles :: SpecWith (ActionState DB)
spec_agentsAndRoles = describe "agentsAndRoles" $ do
    describe "agents and roles" $ do
        describe "assign" $ do
            it "can be called by admins" $ \ sta -> do
                (UserA -> targetAgent, _, _) <- runActionWithClearance dcBottom sta $ addTestUser 1
                result <- runActionWithPrivsE [RoleAdmin] sta $ assignRole targetAgent (RoleBasic RoleAdmin)
                result `shouldSatisfy` isRight

            it "can NOT be called by any non-admin agents" $ \ sta -> do
                let targetAgent = UserA $ UserId 1
                result <- runActionWithPrivsE [targetAgent] sta $ assignRole targetAgent (RoleBasic RoleAdmin)
                result `shouldSatisfy` isLeft

        describe "lookup" $ do
            it "can be called by admins" $ \ sta -> do
                let targetAgent = UserA $ UserId 1
                result <- runActionWithPrivsE [RoleAdmin] sta $ agentRoles targetAgent
                result `shouldSatisfy` isRight

            it "can be called by user for her own roles" $ \ sta -> do
                let targetAgent = UserA $ UserId 1
                result <- runActionWithPrivsE [targetAgent] sta $ agentRoles targetAgent
                result `shouldSatisfy` isRight

            it "can NOT be called by other users" $ \ sta -> do
                let targetAgent = UserA $ UserId 1
                    askingAgent = UserA $ UserId 2
                result <- runActionWithPrivsE [askingAgent] sta $ agentRoles targetAgent
                result `shouldSatisfy` isLeft


spec_session :: SpecWith (ActionState DB)
spec_session = describe "session" $ do
    describe "StartSession" $ do
        it "works" $ \ sta -> do
            result <- runActionE sta $ startThentosSessionByUserName godName godPass
            result `shouldSatisfy` isRight
            return ()

    describe "lookupThentosSession" $ do
        it "works" $ \ sta -> do
            ((ernieId, ernieF, _) : (bertId, _, _) : _)
                <- runActionWithClearance dcTop sta initializeTestUsers

            tok <- runActionWithClearance dcTop sta $
                    startThentosSessionByUserId ernieId (udPassword ernieF)
            v1 <- runActionAsAgent (UserA ernieId) sta (existsThentosSession tok)
            v2 <- runActionAsAgent (UserA bertId)  sta (existsThentosSession tok)

            runActionWithClearance dcTop sta $ endThentosSession tok
            v3 <- runActionAsAgent (UserA ernieId) sta (existsThentosSession tok)
            v4 <- runActionAsAgent (UserA bertId)  sta (existsThentosSession tok)

            (v1, v2, v3, v4) `shouldBe` (True, False, False, False)

-- create user on a custom db type, using Actions defined for our base DB type
spec_customDb :: Spec
spec_customDb = describe "custom db" . before thentosTestConfig $ do
    it "works" $ \ tcfg -> do
        st <- openMemoryState (CustomDB emptyDB 3)
        rng :: MVar ChaChaDRG <- drgNew >>= newMVar
        let sta = ActionState (st, rng, tcfg)
            user = head testUsers

        uid <- runActionWithPrivs [RoleAdmin] sta $ addUser (head testUserForms)
        (uid', user') <- runActionWithPrivs [RoleAdmin] sta $ lookupUser uid
        uid `shouldBe` uid'
        user' `shouldBe` (userPassword .~ (user' ^. userPassword) $ user)
        void . runActionWithPrivs [RoleAdmin] sta $ deleteUser uid
        Left (ActionErrorThentos e) <-
            runActionWithClearanceE dcBottom sta $ lookupUser uid
        e `shouldBe` thentosErrorFromParent NoSuchUser
        return ()

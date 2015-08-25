{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Thentos.ActionSpec where

import Control.Applicative ((<$>))
import Control.Lens ((.~), (^.))
import Control.Monad (void)
import Data.Either (isLeft, isRight)
import LIO.DCLabel (ToCNF, DCLabel, (%%))
import Test.Hspec (Spec, SpecWith, describe, it, before, shouldBe, shouldContain,
                   shouldNotContain, shouldSatisfy, hspec)

import Thentos.Test.Arbitrary ()
import Thentos.Test.Config
import Thentos.Test.Core

import LIO.Missing
import Thentos.Action
import Thentos.Action.Core
import Thentos.Types


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = do
    let b = do
          db@(ActionState (adb, _, _)) <- createActionState thentosTestConfig
          createGod adb
          return db

    describe "Thentos.Action" . before b $ do
        spec_user
        spec_service
        spec_agentsAndRoles
        spec_session


spec_user :: SpecWith ActionState
spec_user = describe "user" $ do
    describe "addUser, lookupUser, deleteUser" $ do
        it "works" $ \ sta -> do
            let user = testUsers !! 0
            uid <- runPrivs [RoleAdmin] sta $ addUser (head testUserForms)
            (uid', user') <- runPrivs [RoleAdmin] sta $ lookupUser uid
            uid' `shouldBe` uid
            user' `shouldBe` (userPassword .~ (user' ^. userPassword) $ user)
            void . runPrivs [RoleAdmin] sta $ deleteUser uid
            Left (ActionErrorThentos NoSuchUser) <-
                runClearanceE dcBottom sta $ lookupUser uid
            return ()

        it "guarantee that user names are unique" $ \ sta -> do
            (_, _, user) <- runClearance dcBottom sta $ addTestUser 1
            let userFormData = UserFormData (user ^. userName)
                                            (UserPass "foo")
                                            (forceUserEmail "new@one.com")
            Left (ActionErrorThentos e) <- runPrivsE [RoleAdmin] sta $
                addUser userFormData
            e `shouldBe` UserNameAlreadyExists

        it "guarantee that user email addresses are unique" $ \ sta -> do
            (_, _, user) <- runClearance dcBottom sta $ addTestUser 1
            let userFormData = UserFormData (UserName "newOne")
                                            (UserPass "foo")
                                            (user ^. userEmail)
            Left (ActionErrorThentos e) <- runPrivsE [RoleAdmin] sta $ addUser userFormData
            e `shouldBe` UserEmailAlreadyExists


    describe "addUsers" $ do
        it "works" $ \ sta -> do
            result <- runPrivs [RoleAdmin] sta $
                addUsers ((testUserForms !!) <$> [2..4])
            result `shouldBe` (UserId <$> [1..3])

        it "rolls back in case of error (adds all or nothing)" $ \ sta -> do
            _ <- runPrivs [RoleAdmin] sta $ addUser (testUserForms !! 4)
            Left (ActionErrorThentos e) <- runPrivsE [RoleAdmin] sta
                $ addUsers ((testUserForms !!) <$> [2..4])
            e `shouldBe` UserNameAlreadyExists
            result <- runPrivs [RoleAdmin] sta allUserIds
            result `shouldBe` (UserId <$> [0..1])

    describe "DeleteUser" $ do
        it "user can delete herself, even if not admin" $ \ sta -> do
            (uid, _, _) <- runClearance dcBottom sta $ addTestUser 3
            result <- runPrivsE [UserA uid] sta $ deleteUser uid
            result `shouldSatisfy` isRight

        it "nobody else but the deleted user and admin can do this" $ \ sta -> do
            (uid,  _, _) <- runClearance dcBottom sta $ addTestUser 3
            (uid', _, _) <- runClearance dcBottom sta $ addTestUser 4
            result <- runPrivsE [UserA uid] sta $ deleteUser uid'
            result `shouldSatisfy` isLeft

    describe "UpdateUser" $ do
        it "changes user if it exists" $ \ sta -> do
            (uid, _, user) <- runClearance dcBottom sta $ addTestUser 1
            runPrivs [UserA uid] sta $
                updateUserField uid (UpdateUserFieldName "fka_user1")

            result <- runPrivs [UserA uid] sta $ lookupUser uid
            result `shouldBe` (UserId 1, userName .~ "fka_user1" $ user)

        it "throws an error if user does not exist" $ \ sta -> do
            Left (ActionErrorThentos e) <- runPrivsE [RoleAdmin] sta $
                updateUserField (UserId 391) (UpdateUserFieldName "moo")
            e `shouldBe` NoSuchUser

    describe "checkPassword" $ do
        it "works" $ \ sta -> do
            void . runA sta $ startThentosSessionByUserId godUid godPass
            void . runA sta $ startThentosSessionByUserName godName godPass


spec_service :: SpecWith ActionState
spec_service = describe "service" $ do
    describe "addService, lookupService, deleteService" $ do
        it "works" $ \ sta -> do
            let addsvc name desc = runClearanceE (UserA godUid %% UserA godUid) sta
                    $ addService (UserA (UserId 0)) name desc
            Right (service1_id, _s1_key) <- addsvc "fake name" "fake description"
            Right (service2_id, _s2_key) <- addsvc "different name" "different description"
            service1 <- runPrivs [RoleAdmin] sta $ lookupService service1_id
            service2 <- runPrivs [RoleAdmin] sta $ lookupService service2_id
            service1 `shouldBe` service1 -- sanity check for reflexivity of Eq
            service1 `shouldSatisfy` (/= service2) -- should have different keys
            void . runPrivs [RoleAdmin] sta $ deleteService service1_id
            Left (ActionErrorThentos NoSuchService) <-
                runPrivsE [RoleAdmin] sta $ lookupService service1_id
            return ()

    describe "autocreateServiceIfMissing" $ do
        it "adds service if missing" $ \ sta -> do
            let owner = UserA $ UserId 0
            sid <- runPrivs [RoleAdmin] sta $ freshServiceId
            allSids <- runPrivs [RoleAdmin] sta allServiceIds
            allSids `shouldNotContain` [sid]
            runPrivs [RoleAdmin] sta $ autocreateServiceIfMissing'P owner sid
            allSids' <- runPrivs [RoleAdmin] sta allServiceIds
            allSids' `shouldContain` [sid]

        it "does nothing if service exists" $ \ sta -> do
            let owner = UserA $ UserId 0
            (sid, _) <- runPrivs [RoleAdmin] sta
                            $ addService owner "fake name" "fake description"
            allSids <- runPrivs [RoleAdmin] sta allServiceIds
            runPrivs [RoleAdmin] sta $ autocreateServiceIfMissing'P owner sid
            allSids' <- runPrivs [RoleAdmin] sta allServiceIds
            allSids `shouldBe` allSids'

spec_agentsAndRoles :: SpecWith ActionState
spec_agentsAndRoles = describe "agentsAndRoles" $ do
    describe "agents and roles" $ do
        describe "assign" $ do
            it "can be called by admins" $ \ sta -> do
                (UserA -> targetAgent, _, _) <- runClearance dcBottom sta $ addTestUser 1
                result <- runPrivsE [RoleAdmin] sta $ assignRole targetAgent (RoleBasic RoleAdmin)
                result `shouldSatisfy` isRight

            it "can NOT be called by any non-admin agents" $ \ sta -> do
                let targetAgent = UserA $ UserId 1
                result <- runPrivsE [targetAgent] sta $ assignRole targetAgent (RoleBasic RoleAdmin)
                result `shouldSatisfy` isLeft

        describe "lookup" $ do
            it "can be called by admins" $ \ sta -> do
                let targetAgent = UserA $ UserId 1
                result <- runPrivsE [RoleAdmin] sta $ agentRoles targetAgent
                result `shouldSatisfy` isRight

            it "can be called by user for her own roles" $ \ sta -> do
                let targetAgent = UserA $ UserId 1
                result <- runPrivsE [targetAgent] sta $ agentRoles targetAgent
                result `shouldSatisfy` isRight

            it "can NOT be called by other users" $ \ sta -> do
                let targetAgent = UserA $ UserId 1
                    askingAgent = UserA $ UserId 2
                result <- runPrivsE [askingAgent] sta $ agentRoles targetAgent
                result `shouldSatisfy` isLeft


spec_session :: SpecWith ActionState
spec_session = describe "session" $ do
    describe "StartSession" $ do
        it "works" $ \ sta -> do
            result <- runAE sta $ startThentosSessionByUserName godName godPass
            result `shouldSatisfy` isRight
            return ()

    describe "lookupThentosSession" $ do
        it "works" $ \ sta -> do
            ((ernieId, ernieF, _) : (bertId, _, _) : _)
                <- runClearance dcTop sta initializeTestUsers

            tok <- runClearance dcTop sta $
                    startThentosSessionByUserId ernieId (udPassword ernieF)
            v1 <- runAsAgent (UserA ernieId) sta (existsThentosSession tok)
            v2 <- runAsAgent (UserA bertId)  sta (existsThentosSession tok)

            runClearance dcTop sta $ endThentosSession tok
            v3 <- runAsAgent (UserA ernieId) sta (existsThentosSession tok)
            v4 <- runAsAgent (UserA bertId)  sta (existsThentosSession tok)

            (v1, v2, v3, v4) `shouldBe` (True, False, False, False)

-- specialize to error type ()
runA :: ActionState -> Action () a -> IO a
runA = runAction

runAE :: ActionState -> Action () a -> IO (Either (ActionError ()) a)
runAE = runActionE

runAsAgent :: Agent -> ActionState -> Action () a -> IO a
runAsAgent = runActionAsAgent

runPrivs :: ToCNF cnf => [cnf] -> ActionState -> Action () a -> IO a
runPrivs = runPrivs

runPrivsE :: ToCNF cnf => [cnf] -> ActionState -> Action () a -> IO (Either (ActionError ()) a)
runPrivsE = runPrivsE

runClearanceE :: DCLabel -> ActionState -> Action () a -> IO (Either (ActionError ()) a)
runClearanceE = runActionWithClearanceE

runClearance :: DCLabel -> ActionState -> Action () a -> IO a
runClearance = runActionWithClearance


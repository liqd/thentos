{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Thentos.ActionSpec where

import Control.Applicative ((<$>))
import Control.Lens ((.~))
import Control.Monad (void)
import Data.Acid.Advanced (query', update')
import Data.Either (isLeft, isRight)
import LIO.DCLabel ((%%))
import Test.Hspec (Spec, SpecWith, describe, it, before, after, shouldBe, shouldSatisfy, hspec)

import LIO.Missing
import Test.Arbitrary ()
import Test.Config
import Test.Core
import Test.Types
import Thentos.Action
import Thentos.Action.Core
import Thentos.Types

import qualified Thentos.Transaction as T  -- FIXME: this shouldn't be here.


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "Thentos.Action" . before setupDB . after teardownDB $ do
    spec_user
    spec_service
    spec_agentsAndRoles
    spec_session


spec_user :: SpecWith DBTS
spec_user = describe "user" $ do
    describe "AddUser, LookupUser, DeleteUser" $ do
        it "works" $ \ (DBTS _ (ActionState (st, _, _))) -> do
            let user = testUsers !! 0
            Right uid <- update' st $ T.AddUser user
            Right (uid', user') <- query' st $ T.LookupUser uid
            uid' `shouldBe` uid
            user' `shouldBe` user
            void . update' st $ T.DeleteUser uid
            u <- query' st $ T.LookupUser uid
            u `shouldBe` Left NoSuchUser

        it "guarantee that user names are unique" $ \ (DBTS _ sta@(ActionState (st, _, _))) -> do
            (_, _, user) <- runActionWithClearance dcBottom sta $ addTestUser 1
            result <- update' st $ T.AddUser (userEmail .~ forceUserEmail "new@one.com" $ user)
            result `shouldBe` Left UserNameAlreadyExists

        it "guarantee that user email addresses are unique" $ \ (DBTS _ sta@(ActionState (st, _, _))) -> do
            (_, _, user) <- runActionWithClearance dcBottom sta $ addTestUser 1
            result <- update' st $ T.AddUser (userName .~ UserName "newone" $ user)
            result `shouldBe` Left UserEmailAlreadyExists

    describe "AddUsers" $ do
        it "works" $ \ (DBTS _ (ActionState (st, _, _))) -> do
            result <- update' st $ T.AddUsers ((testUsers !!) <$> [2..4])
            result `shouldBe` Right (UserId <$> [1..3])

        it "rolls back in case of error (adds all or nothing)" $ \ (DBTS _ (ActionState (st, _, _))) -> do
            _ <- update' st $ T.AddUser (testUsers !! 4)
            Left UserNameAlreadyExists <- update' st $ T.AddUsers ((testUsers !!) <$> [2..4])
            result <- query' st $ T.AllUserIds
            result `shouldBe` Right (UserId <$> [0..1])

    describe "DeleteUser" $ do
        it "user can delete herself, even if not admin" $ \ (DBTS _ sta) -> do
            (uid, _, _) <- runActionWithClearance dcBottom sta $ addTestUser 3
            result <- runActionWithPrivsE [UserA uid] sta $ deleteUser uid
            result `shouldSatisfy` isRight

        it "nobody else but the deleted user and admin can do this" $ \ (DBTS _ sta) -> do
            (uid,  _, _) <- runActionWithClearance dcBottom sta $ addTestUser 3
            (uid', _, _) <- runActionWithClearance dcBottom sta $ addTestUser 4
            result <- runActionWithPrivsE [UserA uid] sta $ deleteUser uid'
            result `shouldSatisfy` isLeft

    describe "UpdateUser" $ do
        it "changes user if it exists" $ \ (DBTS _ sta@(ActionState (st, _, _))) -> do
            (uid, _, user) <- runActionWithClearance dcBottom sta $ addTestUser 1
            result <- update' st $ T.UpdateUserField uid (T.UpdateUserFieldName "fka_user1")
            result `shouldBe` Right ()

            result2 <- query' st $ T.LookupUser uid
            result2 `shouldBe` Right (UserId 1, userName .~ "fka_user1" $ user)

        it "throws an error if user does not exist" $ \ (DBTS _ (ActionState (st, _, _))) -> do
            result <- update' st $ T.UpdateUserField (UserId 391) (T.UpdateUserFieldName "moo")
            result `shouldBe` Left NoSuchUser

    describe "checkPassword" $ do
        it "works" $ \ (DBTS _ sta) -> do
            byId <- runActionE sta $ startThentosSessionByUserId (UserId 0) (UserPass "god")
            byId `shouldSatisfy` isRight
            byName <- runActionE sta $ startThentosSessionByUserName (UserName "god") (UserPass "god")
            byName `shouldSatisfy` isRight


spec_service :: SpecWith DBTS
spec_service = describe "service" $ do
    describe "AddService, LookupService, DeleteService" $ do
        it "works" $ \ (DBTS _ sta@(ActionState (st, _, _))) -> do
            let addsvc name desc = runActionWithClearanceE (UserA godUid %% UserA godUid) sta $ addService (UserA (UserId 0)) name desc
            Right (service1_id, _s1_key) <- addsvc "fake name" "fake description"
            Right (service2_id, _s2_key) <- addsvc "different name" "different description"
            Right service1 <- query' st $ T.LookupService service1_id
            Right service2 <- query' st $ T.LookupService service2_id
            service1 `shouldBe` service1 -- sanity check for reflexivity of Eq
            service1 `shouldSatisfy` (/= service2) -- should have different keys
            void . update' st $ T.DeleteService service1_id
            Left NoSuchService <- query' st $ T.LookupService service1_id
            return ()


spec_agentsAndRoles :: SpecWith DBTS
spec_agentsAndRoles = describe "agentsAndRoles" $ do
    describe "agents and roles" $ do
        describe "assign" $ do
            it "can be called by admins" $ \ (DBTS _ sta) -> do
                (UserA -> targetAgent, _, _) <- runActionWithClearance dcBottom sta $ addTestUser 1
                result <- runActionWithPrivsE [RoleAdmin] sta $ assignRole targetAgent (RoleBasic RoleAdmin)
                result `shouldSatisfy` isRight

            it "can NOT be called by any non-admin agents" $ \ (DBTS _ sta) -> do
                let targetAgent = UserA $ UserId 1
                result <- runActionWithPrivsE [targetAgent] sta $ assignRole targetAgent (RoleBasic RoleAdmin)
                result `shouldSatisfy` isLeft

        describe "lookup" $ do
            it "can be called by admins" $ \ (DBTS _ sta) -> do
                let targetAgent = UserA $ UserId 1
                result <- runActionWithPrivsE [RoleAdmin] sta $ agentRoles targetAgent
                result `shouldSatisfy` isRight

            it "can be called by user for her own roles" $ \ (DBTS _ sta) -> do
                let targetAgent = UserA $ UserId 1
                result <- runActionWithPrivsE [targetAgent] sta $ agentRoles targetAgent
                result `shouldSatisfy` isRight

            it "can NOT be called by other users" $ \ (DBTS _ sta) -> do
                let targetAgent = UserA $ UserId 1
                    askingAgent = UserA $ UserId 2
                result <- runActionWithPrivsE [askingAgent] sta $ agentRoles targetAgent
                result `shouldSatisfy` isLeft


spec_session :: SpecWith DBTS
spec_session = describe "session" $ do
    describe "StartSession" $ do
        it "works" $ \ (DBTS _ sta) -> do
            result <- runActionE sta $ startThentosSessionByUserName godName godPass
            result `shouldSatisfy` isRight
            return ()

    describe "lookupThentosSession" $ do
        it "works" $ \ (DBTS _ astate :: DBTS) -> do
            ((ernieId, ernieF, _) : (bertId, _, _) : _)
                <- runActionWithClearance dcTop astate initializeTestUsers

            tok <- runActionWithClearance dcTop astate $
                    startThentosSessionByUserId ernieId (udPassword ernieF)
            v1 <- runActionAsAgent (UserA ernieId) astate (existsThentosSession tok)
            v2 <- runActionAsAgent (UserA bertId)  astate (existsThentosSession tok)

            runActionWithClearance dcTop astate $ endThentosSession tok
            v3 <- runActionAsAgent (UserA ernieId) astate (existsThentosSession tok)
            v4 <- runActionAsAgent (UserA bertId)  astate (existsThentosSession tok)

            (v1, v2, v3, v4) `shouldBe` (True, False, False, False)

data MyDB = MyDB DB Int

instance AsDB MyDB where
    asDB f (MyDB db n) = (`MyDB` n) <$> f db

instance HasDB MyDB where
    lookupUser' = query'P . T.LookupUser

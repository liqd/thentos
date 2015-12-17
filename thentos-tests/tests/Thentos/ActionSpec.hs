{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE ViewPatterns         #-}

module Thentos.ActionSpec where

import Control.Lens ((.~), (^.))
import Control.Monad (void)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.Either (isLeft, isRight)
import Data.Functor.Infix ((<$$>))
import Data.Pool (withResource)
import Data.Void (Void)
import LIO.DCLabel (ToCNF, DCLabel, (%%), toCNF)
import Test.Hspec (Spec, SpecWith, describe, it, before, shouldBe, shouldContain,
                   shouldNotContain, shouldSatisfy, hspec)

import Thentos.Test.Arbitrary ()
import Thentos.Test.Config
import Thentos.Test.Core
import Thentos.Test.Transaction

import LIO.Missing
import Thentos.Action
import Thentos.Action.Core
import Thentos.Types
import qualified Thentos.Transaction as T


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = do
    let b = do
          db@(ActionState (connPool, _, _)) <- createActionState "test_thentos" thentosTestConfig
          withResource connPool createGod
          return db

    describe "Thentos.Action" . before b $ do
        spec_user
        spec_service
        spec_agentsAndRoles
        spec_session
        spec_captcha


spec_user :: SpecWith ActionState
spec_user = describe "user" $ do
    describe "addUser, lookupConfirmedUser, deleteUser" $ do
        it "works" $ \sta -> do
            let user = testUsers !! 0
            uid <- runPrivs [RoleAdmin] sta $ addUser (head testUserForms)
            (uid', user') <- runPrivs [RoleAdmin] sta $ lookupConfirmedUser uid
            uid' `shouldBe` uid
            user' `shouldBe` (userPassword .~ (user' ^. userPassword) $ user)
            void . runPrivs [RoleAdmin] sta $ deleteUser uid
            Left (ActionErrorThentos NoSuchUser) <-
                runClearanceE dcBottom sta $ lookupConfirmedUser uid
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

    describe "checkPassword" $ do
        it "works" $ \ sta -> do
            void . runA sta $ startThentosSessionByUserId godUid godPass
            void . runA sta $ startThentosSessionByUserName godName godPass

    describe "confirmUserEmailChange" $ do
        it "changes user email after change request" $ \ sta -> do
            let ActionState (conns, _, _) = sta
                newEmail = forceUserEmail "changed@example.com"
                checkEmail uid p = do
                    (_, user) <- runPrivs [RoleAdmin] sta $ lookupConfirmedUser uid
                    user ^. userEmail `shouldSatisfy` p
                runWithoutPrivs = runPrivs ([] :: [Bool])
            (uid, _, _) <- runClearance dcBottom sta $ addTestUser 1
            checkEmail uid $ not . (==) newEmail
            void . runPrivs [UserA uid] sta $ requestUserEmailChange uid newEmail (const "")
            checkEmail uid $ not . (==) newEmail
            [Only token] <- doQuery conns
                [sql| SELECT token FROM email_change_tokens WHERE uid = ?|] (Only uid)
            void . runWithoutPrivs sta $ confirmUserEmailChange token
            checkEmail uid $ (==) newEmail

    describe "addUnconfirmedUserWithCaptcha" $ do
        it "records signup attempts in the database" $ \sta -> do
            let ActionState (conns, _, _) = sta
                name = UserName "someName"
                userData = UserFormData name "secret" (forceUserEmail "me@example.com")
                cid = "myCaptchaId"
                solution = "theSolution"
                captchaSolution = CaptchaSolution cid solution
                req = UserCreationRequest userData captchaSolution
            Right () <- runVoidedQuery conns $ T.storeCaptcha cid solution
            void . runPrivs ([] :: [Bool]) sta $ addUnconfirmedUserWithCaptcha req
            [(name', captchaCorrect)] <- doQuery conns
                [sql| SELECT user_name, captcha_correct
                      FROM signup_attempts |] ()
            name `shouldBe` name'
            captchaCorrect `shouldBe` True

        it "records signup attempts in the database if the captcha is wrong" $ \sta -> do
            let ActionState (conns, _, _) = sta
                name = UserName "someName"
                userData = UserFormData name "secret" (forceUserEmail "me@example.com")
                cid = "myCaptchaId"
                captchaSolution = CaptchaSolution cid "wrong answer"
                req = UserCreationRequest userData captchaSolution
            Right () <- runVoidedQuery conns $ T.storeCaptcha cid "right answer"
            Left _ <- runPrivsE ([] :: [Bool]) sta $ addUnconfirmedUserWithCaptcha req
            [(name', captchaCorrect)] <- doQuery conns
                [sql| SELECT user_name, captcha_correct
                      FROM signup_attempts |] ()
            name `shouldBe` name'
            captchaCorrect `shouldBe` False

spec_service :: SpecWith ActionState
spec_service = describe "service" $ do
    describe "addService, lookupService, deleteService" $ do
        it "works" $ \ sta -> do
            let addsvc name desc = runClearanceE (UserA godUid %% UserA godUid) sta
                    $ addService (UserId 0) name desc
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
            let owner = UserId 0
            sid <- runPrivs [RoleAdmin] sta $ freshServiceId
            allSids <- runPrivs [RoleAdmin] sta allServiceIds
            allSids `shouldNotContain` [sid]
            runPrivs [RoleAdmin] sta $ autocreateServiceIfMissing'P owner sid
            allSids' <- runPrivs [RoleAdmin] sta allServiceIds
            allSids' `shouldContain` [sid]

        it "does nothing if service exists" $ \ sta -> do
            let owner = UserId 0
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
                result <- runPrivsE [RoleAdmin] sta $ assignRole targetAgent RoleAdmin
                result `shouldSatisfy` isRight

            it "can NOT be called by any non-admin agents" $ \ sta -> do
                let targetAgent = UserA $ UserId 1
                result <- runPrivsE [targetAgent] sta $ assignRole targetAgent RoleAdmin
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

spec_captcha :: SpecWith ActionState
spec_captcha = describe "captcha" $ do
    describe "solveCaptcha" $ do
        it "returns true but doesn't delete the captcha if the solution is correct" $ \sta -> do
            let ActionState (conns, _, _) = sta
            void $ runVoidedQuery conns $ T.storeCaptcha cid solution
            captchaResult <- runA sta $ solveCaptcha cid solution
            captchaResult `shouldBe` True
            [Only count] <- doQuery conns
                [sql| SELECT COUNT(*) FROM captchas WHERE id = ?|] (Only cid)
            count `shouldBe` (1 :: Int)
        it "returns false and deletes the captcha if the solution is wrong" $ \sta -> do
            let ActionState (conns, _, _) = sta
            void $ runVoidedQuery conns $ T.storeCaptcha cid solution
            captchaResult <- runA sta $ solveCaptcha cid "wrong"
            captchaResult `shouldBe` False
            [Only count] <- doQuery conns
                [sql| SELECT COUNT(*) FROM captchas WHERE id = ?|] (Only cid)
            count `shouldBe` (0 :: Int)
  where
    cid      = "RandomId"
    solution = "some text"

-- specialize to error type 'Void' and state '()'
runA :: ActionState -> Action Void () a -> IO a
runA = (fst <$$>) . runAction ()

runAE :: ActionState -> Action Void () a -> IO (Either (ActionError Void) a)
runAE = (fst <$$>) . runActionE ()

runAsAgent :: Agent -> ActionState -> Action Void () a -> IO a
runAsAgent agent = (fst <$$>) . runActionAsAgent agent ()

runPrivs :: ToCNF cnf => [cnf] -> ActionState -> Action Void () a -> IO a
runPrivs xs = (fst <$$>) . runActionWithPrivs (toCNF <$> xs) ()

runPrivsE :: ToCNF cnf
        => [cnf] -> ActionState -> Action Void () a -> IO (Either (ActionError Void) a)
runPrivsE xs = (fst <$$>) . runActionWithPrivsE (toCNF <$> xs) ()

runClearanceE :: DCLabel -> ActionState -> Action Void () a -> IO (Either (ActionError Void) a)
runClearanceE l = (fst <$$>) . runActionWithClearanceE l ()

runClearance :: DCLabel -> ActionState -> Action Void () a -> IO a
runClearance l = (fst <$$>) . runActionWithClearance l ()

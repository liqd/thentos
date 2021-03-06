{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE ViewPatterns         #-}

module Thentos.ActionSpec where

import Control.Lens ((.~), (^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.Configifier (Source(YamlString), (>>.))
import Data.Either (isLeft, isRight)
import Data.Functor.Infix ((<$$>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs)
import Data.Void (Void)
import LIO.DCLabel (ToCNF, DCLabel, (%%), toCNF)
import System.FilePath ((</>))
import System.Process (readProcess)
import Test.Hspec (Spec, SpecWith, context, describe, it, around, shouldBe, shouldContain,
                   shouldNotContain, shouldSatisfy, hspec)

import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Csv as CSV
import qualified Data.Vector as V

import Thentos.Test.Arbitrary ()
import Thentos.Test.Config
import Thentos.Test.Core
import Thentos.Test.Transaction
import Thentos (createDefaultUser)

import LIO.Missing
import Thentos.Action
import Thentos.Action.Core
import Thentos.Action.Types
import Thentos.Types

import qualified Thentos.Transaction as T


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = do
    let b action = outsideTempDirectory $ \tmp -> do
          cfg <- thentosTestConfig' [ YamlString . cs . unlines $
                      [ "signup_log: " ++ tmp </> "signups.log"
                      , "log:"
                      , "  level: DEBUG"
                      , "  stdout: False"
                      , "  path: " ++ tmp </> "log" ]]
          as <- createActionEnv' cfg
          createDefaultUser as
          action as

    describe "Thentos.Action" . around b $ do
        spec_user
        spec_service
        spec_agentsAndGroups
        spec_session
        spec_captcha


spec_user :: SpecWith ActionEnv
spec_user = describe "user" $ do
    describe "addUser, lookupConfirmedUser, deleteUser" $ do
        it "works" $ \sta -> do
            let (userForm:_) = testUserForms
            uid <- runPrivs [GroupAdmin] sta $ addUser userForm

            Left (ActionErrorThentos NoSuchUser)
                <- runClearanceE dcBottom sta $ lookupConfirmedUser uid
            (uid', user')
                <- runPrivs [GroupAdmin] sta $ lookupConfirmedUser uid
            uid' `shouldBe` uid
            let clearPassword = userPassword .~ testHashedUserPass
                in clearPassword user' `shouldBe` clearPassword (mkUser userForm)

            void . runPrivs [GroupAdmin] sta $ deleteUser uid
            Left (ActionErrorThentos NoSuchUser) <-
                runPrivsE [GroupAdmin] sta $ lookupConfirmedUser uid
            return ()

        it "guarantee that user names are unique" $ \sta -> do
            [(_, _, user)] <- createTestUsers (sta ^. aStDb) 1
            let userFormData = UserFormData (user ^. userName)
                                            (UserPass "foo")
                                            (forceUserEmail "new@one.com")
            Left (ActionErrorThentos e) <- runPrivsE [GroupAdmin] sta $
                addUser userFormData
            e `shouldBe` UserNameAlreadyExists

        it "guarantee that user email addresses are unique" $ \sta -> do
            [(_, _, user)] <- createTestUsers (sta ^. aStDb) 1
            let userFormData = UserFormData (UserName "newOne")
                                            (UserPass "foobar")
                                            (user ^. userEmail)
            Left (ActionErrorThentos e) <- runPrivsE [GroupAdmin] sta $ addUser userFormData
            e `shouldBe` UserEmailAlreadyExists

    describe "addUnconfirmedUserWithCaptcha" $ do
            let email = forceUserEmail "alice@example.com"
                name = UserName "alice"
                userData = UserFormData name (UserPass "passwd") email

                createCaptcha sta = doTransaction (sta ^. aStDb)
                    [sql| INSERT INTO captchas (id, solution)
                          VALUES ('cid', 'secret')|] ()

                checkSignupLog as captchaStatus = do
                    let logfile :: FilePath
                        Just logfile = cs <$> (as ^. aStConfig) >>. (Proxy :: Proxy '["signup_log"])
                    content <- liftIO $ readProcess "cat" [logfile] ""
                    let bs = BC.pack content
                        Right (records :: V.Vector SignupAttempt) =
                            CSV.decode CSV.NoHeader bs
                        SignupAttempt name' email' captchaAttempt _ = V.last records

                    V.length records `shouldBe` 1
                    name `shouldBe` name'
                    email `shouldBe` email'
                    captchaAttempt `shouldBe` captchaStatus

                aliceExists sta = do
                    [Only (count :: Int)] <- doQuery (sta ^. aStDb)
                        [sql| SELECT COUNT(*) FROM users WHERE name = 'alice' |] ()
                    return $ count == 1

            it "creates a new user if the captcha is correct, logs signup attempt" $ \sta -> do
                _ <- createCaptcha sta
                let userCreationRequest = UserCreationRequest userData captchaSolution
                    captchaSolution = CaptchaSolution (CaptchaId "cid") "secret"
                Right _ <- runPrivsE [GroupAdmin] sta $ addUnconfirmedUserWithCaptcha userCreationRequest
                True <- aliceExists sta
                checkSignupLog sta CaptchaCorrect

            it "doesn't create a user if the captcha is incorrect, logs signup attempt" $ \sta -> do
                _ <- createCaptcha sta
                let userCreationRequest = UserCreationRequest userData captchaSolution
                    captchaSolution = CaptchaSolution (CaptchaId "cid") "wrong"
                Left (ActionErrorThentos InvalidCaptchaSolution) <-
                    runPrivsE [GroupAdmin] sta $ addUnconfirmedUserWithCaptcha userCreationRequest
                False <- aliceExists sta
                checkSignupLog sta CaptchaIncorrect

    describe "DeleteUser" $ do
        it "user can delete herself, even if not admin" $ \sta -> do
            [(uid, _, _)] <- createTestUsers (sta ^. aStDb) 1
            result <- runPrivsE [UserA uid] sta $ deleteUser uid
            result `shouldSatisfy` isRight

        it "nobody else but the deleted user and admin can do this" $ \sta -> do
            [(uid, _, _), (uid', _, _)] <- createTestUsers (sta ^. aStDb) 2
            result <- runPrivsE [UserA uid] sta $ deleteUser uid'
            result `shouldSatisfy` isLeft

    describe "checkPassword" $ do
        it "works" $ \sta -> do
            (godUid, godPass, godName) <- getDefaultUser (sta ^. aStConfig) (sta ^. aStDb)
            void . runA sta $ startThentosSessionByUserId godUid godPass
            void . runA sta $ startThentosSessionByUserName godName godPass

    describe "confirmUserEmailChange" $ do
        it "changes user email after change request" $ \sta -> do
            let newEmail = forceUserEmail "changed@example.com"
                checkEmail uid p = do
                    (_, user) <- runPrivs [GroupAdmin] sta $ lookupConfirmedUser uid
                    user ^. userEmail `shouldSatisfy` p
            [(uid, _, _)] <- createTestUsers (sta ^. aStDb) 1
            checkEmail uid $ not . (==) newEmail
            void . runPrivs [UserA uid] sta $ requestUserEmailChange uid newEmail (const "")
            checkEmail uid $ not . (==) newEmail
            [Only token] <- doQuery (sta ^. aStDb)
                [sql| SELECT token FROM email_change_tokens WHERE uid = ?|] (Only uid)
            void . runWithoutPrivs sta $ confirmUserEmailChange token
            checkEmail uid $ (==) newEmail

    describe "sendPasswordResetMail" $ do
        context "if user exists" $ do
            it "sends email with PasswordResetToken and stores token" $ \sta -> do
                let userData = head testUserForms
                uid <- runPrivs [GroupAdmin] sta $ addUser userData
                void . runWithoutPrivs sta . sendPasswordResetMail Nothing . udEmail $ userData
                [Only token] <- doQuery (sta ^. aStDb)
                    [sql| SELECT token FROM password_reset_tokens WHERE uid = ?|] (Only uid)
                let logPath = cs $ (sta ^. aStConfig) >>. (Proxy :: Proxy '["log", "path"])
                line <- liftIO $ readProcess "grep" [cs $ fromPasswordResetToken token, logPath] ""
                line `shouldContain` "/password_reset/"

        context "if user doesn't exist" $ do
            it "fails with NoSuchUser" $ \sta -> do
                Left (ActionErrorThentos err) <- runClearanceE dcBottom sta .
                    sendPasswordResetMail Nothing . udEmail . head $ testUserForms
                err `shouldBe` NoSuchUser

    describe "resetPasswordAndLogin" $ do
        context "if token is valid" $ do
            it "changes password, logs user in, deletes token" $ \sta -> do
                let userData = head testUserForms
                    email    = udEmail userData
                uid <- runPrivs [GroupAdmin] sta $ addUser userData
                resetTok <- snd <$> runWithoutPrivs sta (addPasswordResetToken email)
                void . runWithoutPrivs sta $ resetPasswordAndLogin resetTok "newpass"
                rowCountShouldBe (sta ^. aStDb) "password_reset_tokens" 0
                -- Check that user can login with new pass
                void . runWithoutPrivs sta $ startThentosSessionByUserId uid "newpass"
                pure ()

        context "if token is not valid" $ do
            it "fails with NoSuchToken" $ \sta -> do
                Left (ActionErrorThentos err) <-
                    runClearanceE dcBottom sta $ resetPasswordAndLogin "dummytoken" "dummypass"
                err `shouldBe` NoSuchToken

        context "if password is too short" $ do
            it "fails with PasswordTooShort" $ \sta -> do
                let userData = head testUserForms
                    email    = udEmail userData
                void . runPrivs [GroupAdmin] sta $ addUser userData
                resetTok <- snd <$> runWithoutPrivs sta (addPasswordResetToken email)
                Left (ActionErrorThentos err) <-
                    runClearanceE dcBottom sta $ resetPasswordAndLogin resetTok "short"
                err `shouldBe` PasswordTooShort

        context "if user is unconfirmed" $ do
            it "confirms (activates) them" $ \sta -> do
                let userData = head testUserForms
                    email    = udEmail userData
                runWithoutPrivs sta $ addUnconfirmedUser userData
                resetTok <- snd <$> runWithoutPrivs sta (addPasswordResetToken email)
                void . runWithoutPrivs sta $ resetPasswordAndLogin resetTok "newpass"
                -- Check that user can login (= is confirmed)
                void . runWithoutPrivs sta $ startThentosSessionByUserEmail email "newpass"
                pure ()

  where
    runWithoutPrivs = runPrivs ([] :: [Bool])

spec_service :: SpecWith ActionEnv
spec_service = describe "service" $ do
    describe "addService, lookupService, deleteService" $ do
        it "works" $ \sta -> do
            (godUid, _, _) <- getDefaultUser (sta ^. aStConfig) (sta ^. aStDb)
            let addsvc name desc = runClearanceE (UserA godUid %% UserA godUid) sta
                    $ addService godUid name desc
            Right (service1_id, _s1_key) <- addsvc "fake name" "fake description"
            Right (service2_id, _s2_key) <- addsvc "different name" "different description"
            service1 <- runPrivs [GroupAdmin] sta $ lookupService service1_id
            service2 <- runPrivs [GroupAdmin] sta $ lookupService service2_id
            service1 `shouldBe` service1 -- sanity check for reflexivity of Eq
            service1 `shouldSatisfy` (/= service2) -- should have different keys
            void . runPrivs [GroupAdmin] sta $ deleteService service1_id
            Left (ActionErrorThentos NoSuchService) <-
                runPrivsE [GroupAdmin] sta $ lookupService service1_id
            return ()

    describe "autocreateServiceIfMissing" $ do
        it "adds service if missing" $ \sta -> do
            (godUid, _, _) <- getDefaultUser (sta ^. aStConfig) (sta ^. aStDb)
            sid <- runPrivs [GroupAdmin] sta $ freshServiceId
            allSids <- runPrivs [GroupAdmin] sta allServiceIds
            allSids `shouldNotContain` [sid]
            runPrivs [GroupAdmin] sta $ autocreateServiceIfMissing godUid sid
            allSids' <- runPrivs [GroupAdmin] sta allServiceIds
            allSids' `shouldContain` [sid]

        it "does nothing if service exists" $ \sta -> do
            (godUid, _, _) <- getDefaultUser (sta ^. aStConfig) (sta ^. aStDb)
            (sid, _) <- runPrivs [GroupAdmin] sta
                            $ addService godUid "fake name" "fake description"
            allSids <- runPrivs [GroupAdmin] sta allServiceIds
            runPrivs [GroupAdmin] sta $ autocreateServiceIfMissing godUid sid
            allSids' <- runPrivs [GroupAdmin] sta allServiceIds
            allSids `shouldBe` allSids'

spec_agentsAndGroups :: SpecWith ActionEnv
spec_agentsAndGroups = describe "agentsAndGroups" $ do
    describe "agents and groups" $ do
        describe "assign" $ do
            it "can be called by admins" $ \sta -> do
                [(UserA -> targetAgent, _, _)] <- createTestUsers (sta ^. aStDb) 1
                result <- runPrivsE [GroupAdmin] sta $ assignGroup targetAgent GroupAdmin
                result `shouldSatisfy` isRight

            it "can NOT be called by any non-admin agents" $ \sta -> do
                [(UserA -> targetAgent, _, _)] <- createTestUsers (sta ^. aStDb) 1
                result <- runPrivsE [targetAgent] sta $ assignGroup targetAgent GroupAdmin
                result `shouldSatisfy` isLeft

        describe "lookup" $ do
            it "can be called by admins" $ \sta -> do
                let targetAgent = UserA $ UserId 1
                result <- runPrivsE [GroupAdmin] sta $ agentGroups targetAgent
                result `shouldSatisfy` isRight

            it "can be called by user for her own groups" $ \sta -> do
                let targetAgent = UserA $ UserId 1
                result <- runPrivsE [targetAgent] sta $ agentGroups targetAgent
                result `shouldSatisfy` isRight

            it "can NOT be called by other users" $ \sta -> do
                let targetAgent = UserA $ UserId 1
                    askingAgent = UserA $ UserId 2
                result <- runPrivsE [askingAgent] sta $ agentGroups targetAgent
                result `shouldSatisfy` isLeft


spec_session :: SpecWith ActionEnv
spec_session = describe "session" $ do
    describe "StartSession" $ do
        it "works" $ \sta -> do
            let (godPass, godName) = getDefaultUser' (sta ^. aStConfig)
            result <- runAE sta $ startThentosSessionByUserName godName godPass
            result `shouldSatisfy` isRight
            return ()

    describe "lookupThentosSession" $ do
        it "works" $ \sta -> do
            [(ernieId, erniePass, _), (bertId, _, _)] <- createTestUsers (sta ^. aStDb) 2

            tok <- runClearance dcTop sta $
                    startThentosSessionByUserId ernieId erniePass
            v1 <- runAsAgent (UserA ernieId) sta (existsThentosSession tok)
            v2 <- runAsAgent (UserA bertId)  sta (existsThentosSession tok)

            runClearance dcTop sta $ endThentosSession tok
            v3 <- runAsAgent (UserA ernieId) sta (existsThentosSession tok)
            v4 <- runAsAgent (UserA bertId)  sta (existsThentosSession tok)

            (v1, v2, v3, v4) `shouldBe` (True, False, False, False)

spec_captcha :: SpecWith ActionEnv
spec_captcha = describe "captcha" $ do
    describe "solveCaptcha" $ do
        it "returns true but doesn't delete the captcha if the solution is correct" $ \sta -> do
            void $ runVoidedQuery (sta ^. aStDb) $ T.storeCaptcha cid solution
            captchaResult <- runA sta $ solveCaptcha cid solution
            captchaResult `shouldBe` True
            [Only count] <- doQuery (sta ^. aStDb)
                [sql| SELECT COUNT(*) FROM captchas WHERE id = ?|] (Only cid)
            count `shouldBe` (1 :: Int)
        it "returns false and deletes the captcha if the solution is wrong" $ \sta -> do
            void $ runVoidedQuery (sta ^. aStDb) $ T.storeCaptcha cid solution
            captchaResult <- runA sta $ solveCaptcha cid "wrong"
            captchaResult `shouldBe` False
            [Only count] <- doQuery (sta ^. aStDb)
                [sql| SELECT COUNT(*) FROM captchas WHERE id = ?|] (Only cid)
            count `shouldBe` (0 :: Int)
  where
    cid      = "RandomId"
    solution = "some-text"

-- specialize to error type 'Void' and state '()'
runA :: ActionEnv -> ActionStack Void () a -> IO a
runA = (fst <$$>) . runAction ()

runAE :: ActionEnv -> ActionStack Void () a -> IO (Either (ActionError Void) a)
runAE = (fst <$$>) . runActionE ()

runAsAgent :: Agent -> ActionEnv -> ActionStack Void () a -> IO a
runAsAgent agent = (fst <$$>) . runActionAsAgent agent ()

runPrivs :: ToCNF cnf => [cnf] -> ActionEnv -> ActionStack Void () a -> IO a
runPrivs xs = (fst <$$>) . runActionWithPrivs (toCNF <$> xs) ()

runPrivsE :: ToCNF cnf
        => [cnf] -> ActionEnv -> ActionStack Void () a -> IO (Either (ActionError Void) a)
runPrivsE xs = (fst <$$>) . runActionWithPrivsE (toCNF <$> xs) ()

runClearanceE :: DCLabel -> ActionEnv -> ActionStack Void () a -> IO (Either (ActionError Void) a)
runClearanceE l = (fst <$$>) . runActionWithClearanceE l ()

runClearance :: DCLabel -> ActionEnv -> ActionStack Void () a -> IO a
runClearance l = (fst <$$>) . runActionWithClearance l ()

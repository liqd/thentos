{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Thentos.TransactionSpec (spec) where

import Control.Lens ((^.))
import Control.Monad (void)
import Data.Either (isRight)
import Data.List (sort)
import Data.Pool (Pool)
import Data.String.Conversions (ST)
import Database.PostgreSQL.Simple (Connection, Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Test.Hspec (Spec, SpecWith, before, describe, it, shouldBe, shouldReturn, shouldSatisfy)

import qualified Data.Set as Set

import Thentos.Transaction
import Thentos.Types
import Thentos.Util (hashUserPass, hashServiceKey)

import Thentos.Test.Core
import Thentos.Test.Config
import Thentos.Test.Transaction

spec :: Spec
spec = describe "Thentos.Transaction" . before (thentosTestConfig >>= createDb)
  $ do
    addUserPrimSpec
    addUserSpec
    addUnconfirmedUserSpec
    finishUserRegistrationSpec
    lookupConfirmedUserByNameSpec
    lookupConfirmedUserByEmailSpec
    lookupAnyUserByEmailSpec
    deleteUserSpec
    passwordResetTokenSpec
    changePasswordSpec
    agentRolesSpec
    assignRoleSpec
    unassignRoleSpec
    addPersonaSpec
    deletePersonaSpec
    addContextSpec
    deleteContextSpec
    registerPersonaWithContextSpec
    unregisterPersonaFromContextSpec
    findPersonaSpec
    contextsForServiceSpec
    addPersonaToGroupSpec
    removePersonaFromGroupSpec
    addGroupToGroupSpec
    removeGroupFromGroupSpec
    personaGroupsSpec
    storeCaptchaSpec
    solveCaptchaSpec
    deleteCaptchaSpec
    garbageCollectUnconfirmedUsersSpec
    garbageCollectPasswordResetTokensSpec
    garbageCollectEmailChangeTokensSpec
    garbageCollectThentosSessionsSpec
    garbageCollectServiceSessionsSpec
    garbageCollectCaptchasSpec
    emailChangeRequestSpec
    addServiceSpec
    deleteServiceSpec
    lookupServiceSpec
    lookupThentosSessionSpec
    startThentosSessionSpec
    endThentosSessionSpec
    serviceNamesFromThentosSessionSpec
    startServiceSessionSpec
    lookupServiceSessionSpec
    endServiceSessionSpec

addUserPrimSpec :: SpecWith (Pool Connection)
addUserPrimSpec = describe "addUserPrim" $ do

    it "adds a confirmed user to the database" $ \connPool -> do
        let user   = testUsers !! 2
            userId = UserId 289
        void $ runVoidedQuery connPool $ addUserPrim (Just userId) user True
        Right (_, res) <- runVoidedQuery connPool $ lookupConfirmedUser userId
        res `shouldBe` user

    it "adds an unconfirmed user to the database" $ \connPool -> do
        let user   = testUsers !! 2
            userId = UserId 290
        void $ runVoidedQuery connPool $ addUserPrim (Just userId) user False
        runVoidedQuery connPool (lookupConfirmedUser userId) `shouldReturn` Left NoSuchUser
        Right (_, res) <- runVoidedQuery connPool $ lookupAnyUser userId
        res `shouldBe` user

    it "fails if the id is not unique" $ \connPool -> do
        let userId = UserId 289
        void $ runVoidedQuery connPool $ addUserPrim (Just userId) (testUsers !! 2) True
        x <- runVoidedQuery connPool $ addUserPrim (Just userId) (testUsers !! 3) True
        x `shouldBe` Left UserIdAlreadyExists

    it "fails if the username is not unique" $ \connPool -> do
        let user1 = mkUser "name" "pass1" "email1@example.com"
            user2 = mkUser "name" "pass2" "email2@example.com"
        void $ runVoidedQuery connPool $ addUserPrim (Just $ UserId 372) user1 True
        x <- runVoidedQuery connPool $ addUserPrim (Just $ UserId 482) user2 True
        x `shouldBe` Left UserNameAlreadyExists

    it "fails if the email is not unique" $  \connPool -> do
        let user1 = mkUser "name1" "pass1" "email@example.com"
            user2 = mkUser "name2" "pass2" "email@example.com"
        void $ runVoidedQuery connPool $ addUserPrim (Just $ UserId 372) user1 True
        x <- runVoidedQuery connPool $ addUserPrim (Just $ UserId 482) user2 True
        x `shouldBe` Left UserEmailAlreadyExists

addUserSpec :: SpecWith (Pool Connection)
addUserSpec = describe "addUser" $ do

    it "adds a user to the database" $ \connPool -> do
        void $ runVoidedQuery connPool $ mapM_ addUser testUsers
        let names = (^. userName) <$> testUsers
        Right res <- runVoidedQuery connPool $ mapM lookupConfirmedUserByName names
        (snd <$> res) `shouldBe` testUsers

addUnconfirmedUserSpec :: SpecWith (Pool Connection)
addUnconfirmedUserSpec = describe "addUnconfirmedUser" $ do
    let user  = mkUser "name" "pass" "email@example.com"
        token = "sometoken"

    it "adds an unconfirmed a user to the database" $ \connPool -> do
        Right uid <- runVoidedQuery connPool $ addUnconfirmedUser token user
        runVoidedQuery connPool (lookupConfirmedUser uid) `shouldReturn` Left NoSuchUser
        Right (_, usr) <- runVoidedQuery connPool $ lookupAnyUser uid
        usr `shouldBe` user

finishUserRegistrationSpec :: SpecWith (Pool Connection)
finishUserRegistrationSpec = describe "finishUserRegistration" $ do
    it "confirms the user if the given token exists" $ \connPool -> do
        Right uid <- runVoidedQuery connPool $ addUnconfirmedUser token testUser
        Right uid' <- runVoidedQuery connPool $ finishUserRegistration timeout token
        uid `shouldBe` uid'
        [Only confirmed] <- doQuery connPool
            [sql| SELECT confirmed FROM users WHERE id = ? |] (Only uid)
        confirmed `shouldBe` True
        rowCountShouldBe connPool "user_confirmation_tokens" 0

    it "fails if the given token does not exist" $ \connPool -> do
        Right uid <- runVoidedQuery connPool $ addUnconfirmedUser token testUser
        Left tok <- runVoidedQuery connPool $ finishUserRegistration timeout "badToken"
        tok `shouldBe` NoSuchPendingUserConfirmation
        [Only confirmed] <- doQuery connPool
            [sql| SELECT confirmed FROM users WHERE id = ? |] (Only uid)
        confirmed `shouldBe` False

  where
    token = "someToken"
    timeout = fromMinutes 1

lookupConfirmedUserByNameSpec :: SpecWith (Pool Connection)
lookupConfirmedUserByNameSpec = describe "lookupConfirmedUserByName" $ do

    it "returns a confirmed user" $ \connPool -> do
        let user = mkUser "name" "pass" "email@example.com"
            userId = UserId 437
        void $ runVoidedQuery connPool $ addUserPrim (Just userId) user True
        runVoidedQuery connPool (lookupConfirmedUserByName "name") `shouldReturn` Right (userId, user)

    it "returns NoSuchUser for unconfirmed users" $ \connPool -> do
        let user = mkUser "name" "pass" "email@example.com"
            userId = UserId 437
        void $ runVoidedQuery connPool $ addUserPrim (Just userId) user False
        runVoidedQuery connPool (lookupConfirmedUserByName "name") `shouldReturn` Left NoSuchUser

    it "returns NoSuchUser if no user has the name" $ \connPool -> do
        runVoidedQuery connPool (lookupConfirmedUserByName "name") `shouldReturn` Left NoSuchUser

lookupConfirmedUserByEmailSpec :: SpecWith (Pool Connection)
lookupConfirmedUserByEmailSpec = describe "lookupConfirmedUserByEmail" $ do

    it "returns a confirmed user" $ \connPool -> do
        let user = mkUser "name" "pass" "email@example.com"
            userId = UserId 437
        void $ runVoidedQuery connPool $ addUserPrim (Just userId) user True
        runVoidedQuery connPool (lookupConfirmedUserByEmail $ forceUserEmail "email@example.com")
            `shouldReturn` Right (userId, user)

    it "returns NoSuchUser for unconfirmed users" $ \connPool -> do
        let user = mkUser "name" "pass" "email@example.com"
            userId = UserId 437
        void $ runVoidedQuery connPool $ addUserPrim (Just userId) user False
        runVoidedQuery connPool (lookupConfirmedUserByEmail $ forceUserEmail "email@example.com")
            `shouldReturn` Left NoSuchUser

    it "returns NoSuchUser if no user has the email" $ \connPool -> do
        runVoidedQuery connPool (lookupConfirmedUserByEmail $ forceUserEmail "email@example.com")
            `shouldReturn` Left NoSuchUser

lookupAnyUserByEmailSpec :: SpecWith (Pool Connection)
lookupAnyUserByEmailSpec = describe "lookupAnyUserByEmail" $ do

    it "returns a confirmed user" $ \connPool -> do
        let user = mkUser "name" "pass" "email@example.com"
            userId = UserId 437
        void $ runVoidedQuery connPool $ addUserPrim (Just userId) user True
        runVoidedQuery connPool (lookupAnyUserByEmail $ forceUserEmail "email@example.com")
            `shouldReturn` Right (userId, user)

    it "returns an unconfirmed user" $ \connPool -> do
        let user = mkUser "name" "pass" "email@example.com"
            userId = UserId 437
        void $ runVoidedQuery connPool $ addUserPrim (Just userId) user False
        runVoidedQuery connPool (lookupAnyUserByEmail $ forceUserEmail "email@example.com")
            `shouldReturn` Right (userId, user)

    it "returns NoSuchUser if no user has the email" $ \connPool -> do
        runVoidedQuery connPool (lookupAnyUserByEmail $ forceUserEmail "email@example.com")
            `shouldReturn` Left NoSuchUser

deleteUserSpec :: SpecWith (Pool Connection)
deleteUserSpec = describe "deleteUser" $ do

    it "deletes a user" $ \connPool -> do
        let user = mkUser "name" "pass" "email@example.com"
            userId = UserId 371
        void $ runVoidedQuery connPool $ addUserPrim (Just userId) user True
        Right _  <- runVoidedQuery connPool $ lookupAnyUser userId
        Right () <- runVoidedQuery connPool $ deleteUser userId
        runVoidedQuery connPool (lookupAnyUser userId) `shouldReturn` Left NoSuchUser

    it "throws NoSuchUser if the id does not exist" $ \connPool -> do
        runVoidedQuery connPool (deleteUser $ UserId 210) `shouldReturn` Left NoSuchUser

passwordResetTokenSpec :: SpecWith (Pool Connection)
passwordResetTokenSpec = describe "addPasswordResetToken" $ do
    it "adds a password reset to the db" $ \connPool -> do
        let user = mkUser "name" "super secret" "me@example.com"
            userId = UserId 584
            testToken = PasswordResetToken "asgbagbaosubgoas"
        Right _ <- runVoidedQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runVoidedQuery connPool $
            addPasswordResetToken (user ^. userEmail) testToken
        [Only token_in_db] <- doQuery connPool
            [sql| SELECT token FROM password_reset_tokens |] ()
        token_in_db `shouldBe` testToken

    it "resets a password if the given token exists" $ \connPool -> do
        let user = mkUser "name" "super secret" "me@example.com"
            userId = UserId 594
            testToken = PasswordResetToken "asgbagbaosubgoas"
        newEncryptedPass <- hashUserPass "newSecretP4ssw0rd"
        Right _ <- runVoidedQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runVoidedQuery connPool $
            addPasswordResetToken (user ^. userEmail) testToken
        Right _ <- runVoidedQuery connPool $
            resetPassword (fromHours 1) testToken newEncryptedPass
        [Only newPassInDB] <- doQuery connPool
            [sql| SELECT password FROM users WHERE id = ?|] (Only userId)
        newPassInDB `shouldBe` newEncryptedPass

changePasswordSpec :: SpecWith (Pool Connection)
changePasswordSpec = describe "changePassword" $ do
    let user = mkUser "name" "super secret" "me@example.com"
        userId = UserId 111
        newPass = encryptTestSecret fromUserPass (UserPass "new")

    it "changes the password" $ \connPool -> do
        Right _ <- runVoidedQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runVoidedQuery connPool $ changePassword userId newPass
        Right (_, usr) <- runVoidedQuery connPool $ lookupConfirmedUser userId
        usr ^. userPassword `shouldBe` newPass

    it "fails if the user doesn't exist" $ \connPool -> do
        Left err <- runVoidedQuery connPool $ changePassword userId newPass
        err `shouldBe` NoSuchUser


agentRolesSpec :: SpecWith (Pool Connection)
agentRolesSpec = describe "agentRoles" $ do
    it "returns an empty set for a user or service without roles" $
      \connPool -> do
        Right _ <- runVoidedQuery connPool $ addUserPrim (Just testUid) testUser True
        x <- runVoidedQuery connPool $ agentRoles (UserA testUid)
        x `shouldBe` Right []

        Right _ <- runVoidedQuery connPool $
            addService testUid sid testHashedSecret "name" "desc"
        roles <- runVoidedQuery connPool $ agentRoles (ServiceA sid)
        roles `shouldBe` Right []
  where
    sid = "sid"

assignRoleSpec :: SpecWith (Pool Connection)
assignRoleSpec = describe "assignRole" $ do
    it "adds a role" $ \connPool -> do
        Right _ <- runVoidedQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runVoidedQuery connPool $ assignRole (UserA testUid) RoleAdmin
        Right roles <- runVoidedQuery connPool $ agentRoles (UserA testUid)
        roles `shouldBe` [RoleAdmin]

        addTestService connPool
        Right _ <- runVoidedQuery connPool $ assignRole (ServiceA sid) RoleAdmin
        Right serviceRoles <- runVoidedQuery connPool $ agentRoles (ServiceA sid)
        serviceRoles `shouldBe` [RoleAdmin]

    it "silently allows adding a duplicate role" $ \connPool -> do
        Right _ <- runVoidedQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runVoidedQuery connPool $ assignRole (UserA testUid) RoleAdmin
        x <- runVoidedQuery connPool $ assignRole (UserA testUid) RoleAdmin
        x `shouldBe` Right ()
        Right roles <- runVoidedQuery connPool $ agentRoles (UserA testUid)
        roles `shouldBe` [RoleAdmin]

        addTestService connPool
        Right () <- runVoidedQuery connPool $ assignRole (ServiceA sid) RoleAdmin
        Right () <- runVoidedQuery connPool $ assignRole (ServiceA sid) RoleAdmin
        Right serviceRoles <- runVoidedQuery connPool $ agentRoles (ServiceA sid)
        serviceRoles `shouldBe` [RoleAdmin]

    it "adds a second role" $ \connPool -> do
        Right _ <- runVoidedQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runVoidedQuery connPool $ assignRole (UserA testUid) RoleAdmin
        Right _ <- runVoidedQuery connPool $ assignRole (UserA testUid) RoleUser
        Right roles <- runVoidedQuery connPool $ agentRoles (UserA testUid)
        Set.fromList roles `shouldBe` Set.fromList [RoleAdmin, RoleUser]

        addTestService connPool
        Right _ <- runVoidedQuery connPool $ assignRole (ServiceA sid) RoleAdmin
        Right _ <- runVoidedQuery connPool $ assignRole (ServiceA sid) RoleUser
        Right serviceRoles <- runVoidedQuery connPool $ agentRoles (ServiceA sid)
        Set.fromList serviceRoles `shouldBe` Set.fromList [RoleAdmin, RoleUser]

  where
    sid = "sid"
    addTestService connPool = void . runVoidedQuery connPool $
        addService testUid sid testHashedSecret "name" "desc"

unassignRoleSpec :: SpecWith (Pool Connection)
unassignRoleSpec = describe "unassignRole" $ do
    it "silently allows removing a non-assigned role" $ \connPool -> do
        Right _ <- runVoidedQuery connPool $ addUserPrim (Just testUid) testUser True
        x <- runVoidedQuery connPool $ unassignRole (UserA testUid) RoleAdmin
        x `shouldBe` Right ()

        addTestService connPool
        res <- runVoidedQuery connPool $ unassignRole (ServiceA sid) RoleAdmin
        res `shouldBe` Right ()

    it "removes the specified role" $ \connPool -> do
        Right _ <- runVoidedQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runVoidedQuery connPool $ assignRole (UserA testUid) RoleAdmin
        Right _ <- runVoidedQuery connPool $ assignRole (UserA testUid) RoleUser
        Right _ <- runVoidedQuery connPool $ unassignRole (UserA testUid) RoleAdmin
        Right roles <- runVoidedQuery connPool $ agentRoles (UserA testUid)
        roles `shouldBe` [RoleUser]

        addTestService connPool
        Right _ <- runVoidedQuery connPool $ assignRole (ServiceA sid) RoleAdmin
        Right _ <- runVoidedQuery connPool $ assignRole (ServiceA sid) RoleUser
        Right _ <- runVoidedQuery connPool $ unassignRole (ServiceA sid) RoleAdmin
        Right serviceRoles <- runVoidedQuery connPool $ agentRoles (ServiceA sid)
        serviceRoles `shouldBe` [RoleUser]

  where
    sid = "sid"
    addTestService connPool = void . runVoidedQuery connPool $
        addService testUid sid testHashedSecret "name" "desc"

emailChangeRequestSpec :: SpecWith (Pool Connection)
emailChangeRequestSpec = describe "addUserEmailChangeToken" $ do
    it "adds an email change token to the db" $ \connPool -> do
        Right _ <- runPooledQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runPooledQuery connPool $
            addUserEmailChangeRequest userId newEmail testToken
        [Only tokenInDb] <- doQuery connPool
            [sql| SELECT token FROM email_change_tokens|] ()
        tokenInDb `shouldBe` testToken

    it "changes a user's email if given a valid token" $ \connPool -> do
        Right _ <- runPooledQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runPooledQuery connPool $
            addUserEmailChangeRequest userId newEmail testToken
        Right _ <-
            runPooledQuery connPool $ confirmUserEmailChange (fromHours 1) testToken
        [Only expectedEmail] <- doQuery connPool
            [sql| SELECT email FROM users WHERE id = ?|] (Only userId)
        expectedEmail `shouldBe` newEmail

    it "throws NoSuchToken if given an invalid token and does not update the email" $ \connPool -> do
        Right _ <- runPooledQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runPooledQuery connPool $
            addUserEmailChangeRequest userId newEmail testToken
        let badToken = ConfirmationToken "badtoken"
        Left NoSuchToken <-
            runPooledQuery connPool $ confirmUserEmailChange (Timeoutms 3600) badToken
        [Only expectedEmail] <- doQuery connPool
            [sql| SELECT email FROM users WHERE id = ?|] (Only userId)
        expectedEmail `shouldBe` forceUserEmail "me@example.com"

  where
    user = mkUser "name" "super secret" "me@example.com"
    userId = UserId 584
    testToken = ConfirmationToken "asgbagbaosubgoas"
    newEmail = forceUserEmail "new@example.com"

addServiceSpec :: SpecWith (Pool Connection)
addServiceSpec = describe "addService" $ do
    it "adds a service to the db" $ \connPool -> do
        Right _ <- runPooledQuery connPool $ addUserPrim (Just uid) user True
        rowCountShouldBe connPool "services" 0
        Right _ <- runPooledQuery connPool $
            addService uid sid testHashedSecret name description
        [(owner', sid', key', name', desc')] <- doQuery connPool
            [sql| SELECT owner_user, id, key, name, description
                  FROM services |] ()
        owner' `shouldBe` uid
        sid' `shouldBe` sid
        key' `shouldBe` testHashedSecret
        name' `shouldBe` name
        desc' `shouldBe` description

  where
    sid = ServiceId "serviceid1"
    uid = UserId 9
    user = mkUser "name" "super secret" "me@example.com"
    name = ServiceName "MyLittleService"
    description = ServiceDescription "it serves"

deleteServiceSpec :: SpecWith (Pool Connection)
deleteServiceSpec = describe "deleteService" $ do
    it "deletes a service" $ \connPool -> do
        Right _ <- runPooledQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runPooledQuery connPool $
            addService testUid sid testHashedSecret "" ""
        Right _ <- runPooledQuery connPool $ deleteService sid
        rowCountShouldBe connPool "services" 0

    it "fails if the service doesn't exist" $ \connPool -> do
        Left NoSuchService <- runPooledQuery connPool $ deleteService sid
        return ()
  where
    sid = ServiceId "blablabla"

lookupServiceSpec :: SpecWith (Pool Connection)
lookupServiceSpec = describe "lookupService" $ do
    it "looks up a service"  $ \connPool -> do
        Right _ <- runPooledQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runPooledQuery connPool $
            addService testUid sid testHashedSecret "name" "desc"

        Right (sid', service) <- runPooledQuery connPool $ lookupService sid
        service ^. serviceKey `shouldBe` testHashedSecret
        sid' `shouldBe` sid
        service ^. serviceName `shouldBe` name
        service ^. serviceDescription `shouldBe` desc
        service ^. serviceOwner `shouldBe` testUid
  where
    sid = ServiceId "blablabla"
    name = "name"
    desc = "desc"

startThentosSessionSpec :: SpecWith (Pool Connection)
startThentosSessionSpec = describe "startThentosSession" $ do
    let tok = "something"
        user = UserA (UserId 55)
        period = fromMinutes 1

    it "creates a thentos session for a confirmed user" $ \connPool -> do
        void $ runVoidedQuery connPool $ addUserPrim (Just testUid) testUser True
        Right () <- runVoidedQuery connPool $ startThentosSession tok (UserA testUid) period
        [Only uid] <- doQuery connPool [sql| SELECT uid
                                             FROM thentos_sessions
                                             WHERE token = ? |] (Only tok)
        uid `shouldBe` testUid

    it "fails if the user is uconfirmed" $ \connPool -> do
        void $ runVoidedQuery connPool $ addUserPrim (Just testUid) testUser False
        x <- runVoidedQuery connPool $ startThentosSession tok user period
        x `shouldBe` Left NoSuchUser

    it "fails if the user doesn't exist" $ \connPool -> do
        x <- runVoidedQuery connPool $ startThentosSession tok user period
        x `shouldBe` Left NoSuchUser

    it "creates a thentos session for a service" $ \connPool -> do
        let sid = "sid"
        void $ runVoidedQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runPooledQuery connPool $
            addService testUid sid testHashedSecret "name" "desc"
        Right () <- runVoidedQuery connPool $ startThentosSession tok (ServiceA sid) period
        [Only sid'] <- doQuery connPool [sql| SELECT sid
                                         FROM thentos_sessions
                                         WHERE token = ? |] (Only tok)
        sid' `shouldBe` sid

    it "fails if the service doesn't exist" $ \connPool -> do
        x <- runVoidedQuery connPool $ startThentosSession tok (ServiceA "sid") period
        x `shouldBe` Left NoSuchService

lookupThentosSessionSpec :: SpecWith (Pool Connection)
lookupThentosSessionSpec = describe "lookupThentosSession" $ do
    let tok = ThentosSessionToken "hellohello"
        userId = UserId 777
        user = mkUser "name" "pass" "email@example.com"
        agent = UserA userId
        period = fromMinutes 1
        sid = "sid"

    it "fails if there is no session" $ \connPool -> do
        void $ runVoidedQuery connPool $ addUserPrim (Just userId) user True
        x <- runVoidedQuery connPool $ lookupThentosSession tok
        x `shouldBe` Left NoSuchThentosSession

    it "reads back a fresh session" $ \connPool -> do
        void $ runVoidedQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runVoidedQuery connPool $ startThentosSession tok agent period
        Right (t, s) <- runVoidedQuery connPool $ lookupThentosSession tok
        t `shouldBe` tok
        s ^. thSessAgent `shouldBe` agent

        let tok2 = "anothertoken"
        Right _ <- runPooledQuery connPool $
            addService userId sid testHashedSecret "name" "desc"
        Right _ <- runVoidedQuery connPool $ startThentosSession tok2 (ServiceA sid) period
        Right (tok2', sess) <- runVoidedQuery connPool $ lookupThentosSession tok2
        tok2' `shouldBe` tok2
        sess ^. thSessAgent `shouldBe` ServiceA sid

    it "fails for an expired session" $ \connPool -> do
        void $ runVoidedQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runVoidedQuery connPool $ startThentosSession tok agent (fromSeconds 0)
        x <- runVoidedQuery connPool $ lookupThentosSession tok
        x `shouldBe` Left NoSuchThentosSession

    it "extends the session" $ \connPool -> do
        void $ runVoidedQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runVoidedQuery connPool $ startThentosSession tok agent period
        Right (_, sess1) <- runVoidedQuery connPool $ lookupThentosSession tok
        Right (t, sess2) <- runVoidedQuery connPool $ lookupThentosSession tok
        t `shouldBe` tok
        sess1 ^. thSessEnd `shouldSatisfy` (< sess2 ^. thSessEnd)

endThentosSessionSpec :: SpecWith (Pool Connection)
endThentosSessionSpec = describe "endThentosSession" $ do
    let tok = "something"
        userId = UserId 777
        user = mkUser "name" "pass" "email@example.com"
        agent = UserA userId
        period = fromMinutes 1

    it "deletes a session" $ \connPool -> do
        void $ runVoidedQuery connPool $ addUserPrim (Just userId) user True
        void $ runVoidedQuery connPool $ startThentosSession tok agent period
        Right _ <- runVoidedQuery connPool $ lookupThentosSession tok
        Right _ <- runVoidedQuery connPool $ endThentosSession tok
        x <- runVoidedQuery connPool $ lookupThentosSession tok
        x `shouldBe` Left NoSuchThentosSession

    it "silently allows deleting a non-existing session" $ \connPool -> do
        x <- runVoidedQuery connPool $ endThentosSession tok
        x `shouldSatisfy` isRight

serviceNamesFromThentosSessionSpec :: SpecWith (Pool Connection)
serviceNamesFromThentosSessionSpec = describe "serviceNamesFromThentosSession" $ do
    it "gets the names of all services that a thentos session is signed into" $ \conn -> do
        let go = void . runVoidedQuery conn
        go $ addUserPrim (Just testUid) testUser True
        go $ addService testUid "sid1" testHashedSecret "s1-name" "s1-desc"
        go $ addService testUid "sid2" testHashedSecret "s2-name" "s2-desc"
        go $ addService testUid "sid3" testHashedSecret "s3-name" "s3-desc"
        go $ startThentosSession thentosSessionToken (UserA testUid) period
        go $ startServiceSession thentosSessionToken "sst1" "sid1" period
        go $ startServiceSession thentosSessionToken "sst2" "sid2" period
        Right names <- runVoidedQuery conn $ serviceNamesFromThentosSession thentosSessionToken
        sort names `shouldBe` ["s1-name", "s2-name"]
        return ()
  where
    thentosSessionToken = "abcde"
    period = fromMinutes 1

startServiceSessionSpec :: SpecWith (Pool Connection)
startServiceSessionSpec = describe "startServiceSession" $ do
    it "starts a service session" $ \connPool -> do
        void $ runVoidedQuery connPool $ addUserPrim (Just testUid) testUser True
        void $ runVoidedQuery connPool $
            startThentosSession thentosSessionToken (UserA testUid) period
        void $ runVoidedQuery connPool $
            addService testUid sid testHashedSecret "" ""
        void $ runVoidedQuery connPool $
            startServiceSession thentosSessionToken serviceSessionToken sid period
        rowCountShouldBe connPool "service_sessions" 1
  where
    period = fromMinutes 1
    thentosSessionToken = "foo"
    serviceSessionToken = "bar"
    sid = "sid"

endServiceSessionSpec :: SpecWith (Pool Connection)
endServiceSessionSpec = describe "endServiceSession" $ do
    it "ends an service session" $ \connPool -> do
        void $ runVoidedQuery connPool $ addUserPrim (Just testUid) testUser True
        void $ runVoidedQuery connPool $
            startThentosSession thentosSessionToken (UserA testUid) period
        void $ runVoidedQuery connPool $
            addService testUid sid testHashedSecret "" ""
        void $ runVoidedQuery connPool $
            startServiceSession thentosSessionToken serviceSessionToken sid period
        rowCountShouldBe connPool "service_sessions" 1
        void $ runVoidedQuery connPool $ endServiceSession serviceSessionToken
        rowCountShouldBe connPool "service_sessions" 0
  where
    period = fromMinutes 1
    thentosSessionToken = "foo"
    serviceSessionToken = "bar"
    sid = "sid"

lookupServiceSessionSpec :: SpecWith (Pool Connection)
lookupServiceSessionSpec = describe "lookupServiceSession" $ do
    it "looks up the service session with a given token" $ \connPool -> do
        void $ runVoidedQuery connPool $ addUserPrim (Just testUid) testUser True
        void $ runVoidedQuery connPool $
            startThentosSession thentosSessionToken (UserA testUid) period
        void $ runVoidedQuery connPool $
            addService testUid sid testHashedSecret "" ""
        void $ runVoidedQuery connPool $
            startServiceSession thentosSessionToken serviceSessionToken sid period
        Right (tok, sess) <- runVoidedQuery connPool $ lookupServiceSession serviceSessionToken
        sess ^. srvSessService `shouldBe` sid
        sess ^. srvSessExpirePeriod `shouldBe` period
        tok `shouldBe` serviceSessionToken

    it "returns NoSuchServiceSession error if no service with the given id exists" $
      \connPool -> do
        Left err <- runVoidedQuery connPool $ lookupServiceSession "non-existent token"
        err `shouldBe` NoSuchServiceSession

  where
    period = fromMinutes 1
    thentosSessionToken = "foo"
    serviceSessionToken = "bar"
    sid = "sid"


-- * personas, contexts, and groups

addPersonaSpec :: SpecWith (Pool Connection)
addPersonaSpec = describe "addPersona" $ do
    it "adds a persona to the DB" $ \connPool -> do
        Right uid <- runVoidedQuery connPool $ addUser (head testUsers)
        rowCountShouldBe connPool "personas" 0
        Right persona <- runPooledQuery connPool $ addPersona persName uid Nothing
        persona ^. personaName `shouldBe` persName
        persona ^. personaUid `shouldBe` uid
        [(id', name', uid')] <- doQuery connPool [sql| SELECT id, name, uid FROM personas |] ()
        id' `shouldBe` persona ^. personaId
        name' `shouldBe` persName
        uid' `shouldBe` uid

    it "throws NoSuchUser if the persona belongs to a non-existent user" $ \connPool -> do
        Left err <- runVoidedQuery connPool $ addPersona persName (UserId 6696) Nothing
        err `shouldBe` NoSuchUser

    it "throws PersonaNameAlreadyExists if the name is not unique" $ \connPool -> do
        Right uid <- runVoidedQuery connPool $ addUser (head testUsers)
        Right _   <- runVoidedQuery connPool $ addPersona persName uid Nothing
        Left err  <- runVoidedQuery connPool $ addPersona persName uid Nothing
        err `shouldBe` PersonaNameAlreadyExists

deletePersonaSpec :: SpecWith (Pool Connection)
deletePersonaSpec = describe "deletePersona" $ do
    it "deletes a persona" $ \connPool -> do
        Right uid <- runVoidedQuery connPool $ addUser (head testUsers)
        rowCountShouldBe connPool "personas" 0
        Right persona <- runPooledQuery connPool $ addPersona persName uid Nothing
        Right () <- runVoidedQuery connPool . deletePersona $ persona ^. personaId
        rowCountShouldBe connPool "personas" 0

    it "throws NoSuchPersona if the persona does not exist" $ \connPool -> do
        Left err <- runVoidedQuery connPool . deletePersona $ PersonaId 5432
        err `shouldBe` NoSuchPersona

addContextSpec :: SpecWith (Pool Connection)
addContextSpec = describe "addContext" $ do
    it "adds a context with URL to the DB" $ \connPool -> do
        Right uid <- runVoidedQuery connPool $ addUser (head testUsers)
        Right ()  <- runVoidedQuery connPool $
                        addService uid servId testHashedSecret "sName" "sDescription"
        rowCountShouldBe connPool "contexts" 0
        Right cxt <- runPooledQuery connPool $ addContext servId cxtName cxtDesc (Just cxtUrl)
        cxt ^. contextService `shouldBe` servId
        cxt ^. contextName `shouldBe` cxtName
        cxt ^. contextDescription `shouldBe` cxtDesc
        cxt ^. contextUrl `shouldBe` Just cxtUrl
        [(id', name, sid, desc, url)] <- doQuery connPool
            [sql| SELECT id, name, owner_service, description, url FROM contexts |] ()
        id' `shouldBe` cxt ^. contextId
        name `shouldBe` cxtName
        sid `shouldBe` servId
        desc `shouldBe` cxtDesc
        url `shouldBe` Just cxtUrl

    it "adds a context without URL to the DB" $ \connPool -> do
        Right uid <- runVoidedQuery connPool $ addUser (head testUsers)
        Right ()  <- runVoidedQuery connPool $
                        addService uid servId testHashedSecret "sName" "sDescription"
        rowCountShouldBe connPool "contexts" 0
        Right cxt <- runPooledQuery connPool $ addContext servId cxtName cxtDesc Nothing
        cxt ^. contextService `shouldBe` servId
        cxt ^. contextName `shouldBe` cxtName
        cxt ^. contextDescription `shouldBe` cxtDesc
        cxt ^. contextUrl `shouldBe` Nothing
        [(id', name, sid, desc, url)] <- doQuery connPool
            [sql| SELECT id, name, owner_service, description, url FROM contexts |] ()
        id' `shouldBe` cxt ^. contextId
        name `shouldBe` cxtName
        sid `shouldBe` servId
        desc `shouldBe` cxtDesc
        url `shouldBe` (Nothing :: Maybe ProxyUri)

    it "throws NoSuchService if the context belongs to a non-existent service" $ \connPool -> do
        Left err <- runVoidedQuery connPool $ addContext servId cxtName cxtDesc (Just cxtUrl)
        err `shouldBe` NoSuchService

    it "throws ContextNameAlreadyExists if the name is not unique" $ \connPool -> do
        Right uid <- runVoidedQuery connPool $ addUser (head testUsers)
        Right ()  <- runVoidedQuery connPool $
                        addService uid servId testHashedSecret "sName" "sDescription"
        Right _   <- runVoidedQuery connPool $ addContext servId cxtName cxtDesc Nothing
        Left err  <- runVoidedQuery connPool $ addContext servId cxtName cxtDesc Nothing
        err `shouldBe` ContextNameAlreadyExists

deleteContextSpec :: SpecWith (Pool Connection)
deleteContextSpec = describe "deleteContext" $ do
    it "deletes a context from the DB" $ \connPool -> do
        Right uid <- runVoidedQuery connPool $ addUser (head testUsers)
        Right ()  <- runVoidedQuery connPool $
                        addService uid servId testHashedSecret "sName" "sDescription"
        rowCountShouldBe connPool "contexts" 0
        Right _   <- runPooledQuery connPool $ addContext servId cxtName cxtDesc (Just cxtUrl)
        Right ()  <- runPooledQuery connPool $ deleteContext servId cxtName
        rowCountShouldBe connPool "contexts" 0

    it "throws NoSuchContext if the context doesn't exist" $ \connPool -> do
        Left err <- runVoidedQuery connPool $ deleteContext servId "not-a-context"
        err `shouldBe` NoSuchContext

registerPersonaWithContextSpec :: SpecWith (Pool Connection)
registerPersonaWithContextSpec = describe "registerPersonaWithContext" $ do
    it "connects a persona with a context" $ \connPool -> do
        Right uid     <- runVoidedQuery connPool $ addUser (head testUsers)
        Right persona <- runPooledQuery connPool $ addPersona persName uid Nothing
        Right ()      <- runVoidedQuery connPool $
                            addService uid servId testHashedSecret "sName" "sDescription"
        Right cxt     <- runPooledQuery connPool $ addContext servId cxtName cxtDesc Nothing
        Right ()      <- runPooledQuery connPool $
                            registerPersonaWithContext persona servId cxtName
        [(pid, cid)] <- doQuery connPool
                            [sql| SELECT persona_id, context_id FROM personas_per_context |] ()
        cid `shouldBe` cxt ^. contextId
        pid `shouldBe` persona ^. personaId

    it "throws MultiplePersonasPerContext if the persona is already registered" $ \connPool -> do
        Right uid     <- runVoidedQuery connPool $ addUser (head testUsers)
        Right persona <- runPooledQuery connPool $ addPersona persName uid Nothing
        Right ()      <- runVoidedQuery connPool $
                            addService uid servId testHashedSecret "sName" "sDescription"
        Right _       <- runPooledQuery connPool $ addContext servId cxtName cxtDesc (Just cxtUrl)
        Right ()      <- runPooledQuery connPool $
                            registerPersonaWithContext persona servId cxtName
        Left err      <- runVoidedQuery connPool $
                            registerPersonaWithContext persona servId cxtName
        err `shouldBe` MultiplePersonasPerContext

    it "throws MultiplePersonasPerContext if the user registered another persona" $ \connPool -> do
        Right uid      <- runVoidedQuery connPool $ addUser (head testUsers)
        Right persona  <- runPooledQuery connPool $ addPersona persName uid Nothing
        Right persona' <- runPooledQuery connPool $ addPersona "MyMyMy" uid Nothing
        Right ()  <- runVoidedQuery connPool $
                            addService uid servId testHashedSecret "sName" "sDescription"
        Right _   <- runPooledQuery connPool $ addContext servId cxtName cxtDesc Nothing
        Right ()  <- runPooledQuery connPool $ registerPersonaWithContext persona servId cxtName
        Left err  <- runVoidedQuery connPool $ registerPersonaWithContext persona' servId cxtName
        err `shouldBe` MultiplePersonasPerContext

    it "throws NoSuchPersona if the persona doesn't exist" $ \connPool -> do
        Right uid <- runVoidedQuery connPool $ addUser (head testUsers)
        let persona = Persona (PersonaId 5904) persName uid Nothing
        Right () <- runVoidedQuery connPool $
                            addService uid servId testHashedSecret "sName" "sDescription"
        Right _  <- runPooledQuery connPool $ addContext servId cxtName cxtDesc (Just cxtUrl)
        Left err <- runVoidedQuery connPool $ registerPersonaWithContext persona servId cxtName
        err `shouldBe` NoSuchPersona

    it "throws NoSuchContext if the context doesn't exist" $ \connPool -> do
        Right uid     <- runVoidedQuery connPool $ addUser (head testUsers)
        Right persona <- runPooledQuery connPool $ addPersona persName uid Nothing
        Right ()      <- runVoidedQuery connPool $
                            addService uid servId testHashedSecret "sName" "sDescription"
        Left err      <- runVoidedQuery connPool $
                            registerPersonaWithContext persona servId "not-a-context"
        err `shouldBe` NoSuchContext

unregisterPersonaFromContextSpec :: SpecWith (Pool Connection)
unregisterPersonaFromContextSpec = describe "unregisterPersonaFromContext" $ do
    it "disconnects a persona from a context" $ \connPool -> do
        Right uid     <- runVoidedQuery connPool $ addUser (head testUsers)
        Right persona <- runPooledQuery connPool $ addPersona persName uid Nothing
        Right ()      <- runVoidedQuery connPool $
                            addService uid servId testHashedSecret "sName" "sDescription"
        Right cxt <- runPooledQuery connPool $ addContext servId cxtName cxtDesc Nothing
        Right ()  <- runPooledQuery connPool $ registerPersonaWithContext persona servId cxtName
        [Only entryCount]  <- doQuery connPool countEntries (Only $ cxt ^. contextId)
        entryCount `shouldBe` (1 :: Int)
        Right ()      <- runPooledQuery connPool $ unregisterPersonaFromContext
                            (persona ^. personaId) servId cxtName
        [Only entryCount'] <- doQuery connPool countEntries (Only $ cxt ^. contextId)
        entryCount' `shouldBe` (0 :: Int)

    it "is a no-op if persona and context don't exist" $ \connPool -> do
        rowCountShouldBe connPool "personas_per_context" 0
        Right ()      <- runPooledQuery connPool $ unregisterPersonaFromContext
                            (PersonaId 5432) servId "no-such-context"
        rowCountShouldBe connPool "personas_per_context" 0

  where
    countEntries = [sql| SELECT COUNT(*) FROM personas_per_context WHERE context_id = ? |]

findPersonaSpec :: SpecWith (Pool Connection)
findPersonaSpec = describe "findPersona" $ do
    it "find the persona a user wants to use for a context" $ \connPool -> do
        Right uid     <- runVoidedQuery connPool $ addUser (head testUsers)
        Right persona <- runPooledQuery connPool $ addPersona persName uid Nothing
        Right ()      <- runVoidedQuery connPool $
                            addService uid servId testHashedSecret "sName" "sDescription"
        Right _     <- runPooledQuery connPool $ addContext servId cxtName cxtDesc (Just cxtUrl)
        Right ()    <- runPooledQuery connPool $ registerPersonaWithContext persona servId cxtName
        Right mPers <- runPooledQuery connPool $ findPersona uid servId cxtName
        mPers `shouldBe` Just persona

    it "doesn't find a persona if none was registered" $ \connPool -> do
        Right uid   <- runVoidedQuery connPool $ addUser (head testUsers)
        Right _     <- runPooledQuery connPool $ addPersona persName uid Nothing
        Right ()    <- runVoidedQuery connPool $
                            addService uid servId testHashedSecret "sName" "sDescription"
        Right _     <- runPooledQuery connPool $ addContext servId cxtName cxtDesc Nothing
        Right mPers <- runPooledQuery connPool $ findPersona uid servId cxtName
        mPers `shouldBe` Nothing

contextsForServiceSpec :: SpecWith (Pool Connection)
contextsForServiceSpec = describe "contextsForService" $ do
    it "finds contexts registered for a service" $ \connPool -> do
        Right uid  <- runVoidedQuery connPool $ addUser (head testUsers)
        Right ()   <- runVoidedQuery connPool $
                            addService uid servId testHashedSecret "sName" "sDescription"
        Right cxt1 <- runPooledQuery connPool $ addContext servId cxtName cxtDesc (Just cxtUrl)
        Right cxt2 <- runPooledQuery connPool . addContext servId "MeinMoabit"
                            "Another context" . Just $ ProxyUri "example.org" 80 "/mmoabit"
        Right contexts <- runPooledQuery connPool $ contextsForService servId
        Set.fromList contexts `shouldBe` Set.fromList [cxt1, cxt2]

    it "doesn't return contexts registered for other services" $ \connPool -> do
        Right uid  <- runVoidedQuery connPool $ addUser (head testUsers)
        Right ()   <- runVoidedQuery connPool $
                            addService uid servId testHashedSecret "sName" "sDescription"
        Right ()   <- runVoidedQuery connPool $
                            addService uid "sid2" testHashedSecret "s2Name" "s2Description"
        Right _    <- runPooledQuery connPool $ addContext servId cxtName cxtDesc Nothing
        Right _    <- runPooledQuery connPool . addContext servId "MeinMoabit"
                            "Another context" . Just $ ProxyUri "example.org" 80 "/mmoabit"
        Right contexts <- runPooledQuery connPool $ contextsForService "sid2"
        contexts `shouldBe` []

addPersonaToGroupSpec :: SpecWith (Pool Connection)
addPersonaToGroupSpec = describe "addPersonaToGroup" $ do
    it "adds a persona to a group" $ \connPool -> do
        Right uid     <- runVoidedQuery connPool $ addUser (head testUsers)
        Right persona <- runVoidedQuery connPool $ addPersona persName uid Nothing
        rowCountShouldBe connPool "persona_groups" 0
        Right ()      <- runVoidedQuery connPool $ addPersonaToGroup (persona ^. personaId) "dummy"
        [(pid, grp)] <- doQuery connPool [sql| SELECT pid, grp FROM persona_groups |] ()
        pid `shouldBe` persona ^. personaId
        grp `shouldBe` ("dummy" :: ServiceGroup)

    it "no-op if the persona is already a member of the group" $ \connPool -> do
        Right uid     <- runVoidedQuery connPool $ addUser (head testUsers)
        Right persona <- runVoidedQuery connPool $ addPersona persName uid Nothing
        Right ()      <- runVoidedQuery connPool $ addPersonaToGroup (persona ^. personaId) "dummy"
        rowCountShouldBe connPool "persona_groups" 1
        Right ()      <- runVoidedQuery connPool $ addPersonaToGroup (persona ^. personaId) "dummy"
        rowCountShouldBe connPool "persona_groups" 1

removePersonaFromGroupSpec :: SpecWith (Pool Connection)
removePersonaFromGroupSpec = describe "removePersonaFromGroup" $ do
    it "removes a persona from a group" $ \connPool -> do
        Right uid     <- runVoidedQuery connPool $ addUser (head testUsers)
        Right persona <- runVoidedQuery connPool $ addPersona persName uid Nothing
        Right ()      <- runVoidedQuery connPool $ addPersonaToGroup (persona ^. personaId) "dummy"
        Right ()      <- runVoidedQuery connPool $
            removePersonaFromGroup (persona ^. personaId) "dummy"
        rowCountShouldBe connPool "persona_groups" 0

    it "no-op if the persona is not a member of the group" $ \connPool -> do
        Right uid     <- runVoidedQuery connPool $ addUser (head testUsers)
        Right persona <- runVoidedQuery connPool $ addPersona persName uid Nothing
        Right ()      <- runVoidedQuery connPool $
            removePersonaFromGroup (persona ^. personaId) "dummy"
        return ()

addGroupToGroupSpec :: SpecWith (Pool Connection)
addGroupToGroupSpec = describe "addGroupToGroup" $ do
    it "adds a group to another group" $ \connPool -> do
        rowCountShouldBe connPool "group_tree" 0
        Right () <- runVoidedQuery connPool $ addGroupToGroup "admin" "user"
        [(supergroup, subgroup)] <- doQuery connPool
            [sql| SELECT supergroup, subgroup FROM group_tree |] ()
        supergroup `shouldBe` ("user" :: ServiceGroup)
        subgroup `shouldBe` ("admin" :: ServiceGroup)

    it "no-op if subgroup is already a direct member of supergroup" $ \connPool -> do
        Right () <- runVoidedQuery connPool $ addGroupToGroup "admin" "user"
        rowCountShouldBe connPool "group_tree" 1
        Right () <- runVoidedQuery connPool $ addGroupToGroup "admin" "user"
        rowCountShouldBe connPool "group_tree" 1

    it "throws GroupMembershipLoop if a direct loop would result" $ \connPool -> do
        Right () <- runVoidedQuery connPool $ addGroupToGroup "admin" "user"
        Left err <- runVoidedQuery connPool $ addGroupToGroup "user" "admin"
        err `shouldBe` GroupMembershipLoop "user" "admin"

    it "throws GroupMembershipLoop if an indirect loop would result" $ \connPool -> do
        Right () <- runVoidedQuery connPool $ addGroupToGroup "admin" "trustedUser"
        Right () <- runVoidedQuery connPool $ addGroupToGroup "trustedUser" "user"
        Left err <- runVoidedQuery connPool $ addGroupToGroup "user" "admin"
        err `shouldBe` GroupMembershipLoop "user" "admin"

removeGroupFromGroupSpec :: SpecWith (Pool Connection)
removeGroupFromGroupSpec = describe "removeGroupFromGroup" $ do
    it "removes a group from another group" $ \connPool -> do
        Right () <- runVoidedQuery connPool $ addGroupToGroup "admin" "user"
        rowCountShouldBe connPool "group_tree" 1
        Right () <- runVoidedQuery connPool $ removeGroupFromGroup "admin" "user"
        rowCountShouldBe connPool "group_tree" 0

    it "no-op if subgroup is not a direct member of supergroup" $ \connPool -> do
        Right () <- runVoidedQuery connPool $ removeGroupFromGroup "admin" "user"
        return ()

    it "no-op if the relation is the other way around" $ \connPool -> do
        Right () <- runVoidedQuery connPool $ addGroupToGroup "admin" "user"
        rowCountShouldBe connPool "group_tree" 1
        Right () <- runVoidedQuery connPool $ removeGroupFromGroup "user" "admin"
        rowCountShouldBe connPool "group_tree" 1

personaGroupsSpec :: SpecWith (Pool Connection)
personaGroupsSpec = describe "personaGroups" $ do
    it "lists all groups a persona belongs to" $ \connPool -> do
        Right uid     <- runVoidedQuery connPool $ addUser (head testUsers)
        Right persona <- runVoidedQuery connPool $ addPersona persName uid Nothing
        let pid = persona ^. personaId
        Right ()      <- runVoidedQuery connPool $ addPersonaToGroup pid "admin"
        Right ()      <- runVoidedQuery connPool $ addPersonaToGroup pid "user"
        Right groups  <- runVoidedQuery connPool $ personaGroups pid
        Set.fromList groups `shouldBe` Set.fromList ["admin" :: ServiceGroup, "user"]

    it "includes indirect group memberships" $ \connPool -> do
        Right uid     <- runVoidedQuery connPool $ addUser (head testUsers)
        Right persona <- runVoidedQuery connPool $ addPersona persName uid Nothing
        let pid = persona ^. personaId
        Right ()      <- runVoidedQuery connPool $ addPersonaToGroup pid "admin"
        Right ()      <- runVoidedQuery connPool $ addGroupToGroup "admin" "trustedUser"
        Right ()      <- runVoidedQuery connPool $ addGroupToGroup "trustedUser" "user"
        Right groups  <- runVoidedQuery connPool $ personaGroups pid
        Set.fromList groups `shouldBe` Set.fromList ["admin" :: ServiceGroup, "user", "trustedUser"]

    it "eliminates duplicates" $ \connPool -> do
        Right uid     <- runVoidedQuery connPool $ addUser (head testUsers)
        Right persona <- runVoidedQuery connPool $ addPersona persName uid Nothing
        let pid = persona ^. personaId
        Right ()      <- runVoidedQuery connPool $ addPersonaToGroup pid "admin"
        Right ()      <- runVoidedQuery connPool $ addPersonaToGroup pid "user"
        Right ()      <- runVoidedQuery connPool $ addGroupToGroup "admin" "trustedUser"
        Right ()      <- runVoidedQuery connPool $ addGroupToGroup "trustedUser" "user"
        Right groups  <- runVoidedQuery connPool $ personaGroups pid
        length groups `shouldBe` length (Set.fromList groups)
        Set.fromList groups `shouldBe` Set.fromList ["admin" :: ServiceGroup, "user", "trustedUser"]

    it "lists no groups if a persona doesn't belong to any" $ \connPool -> do
        Right uid     <- runVoidedQuery connPool $ addUser (head testUsers)
        Right persona <- runVoidedQuery connPool $ addPersona persName uid Nothing
        let pid = persona ^. personaId
        Right groups  <- runVoidedQuery connPool $ personaGroups pid
        length groups `shouldBe` 0


-- * Sybil attack prevention

storeCaptchaSpec :: SpecWith (Pool Connection)
storeCaptchaSpec = describe "storeCaptcha" $ do
    it "stores a CaptchaId and solution" $ \connPool -> do
        Right () <- runVoidedQuery connPool $ storeCaptcha cid solution
        [(cid', solution')] <- doQuery connPool [sql| SELECT id, solution FROM captchas |] ()
        cid' `shouldBe` cid
        solution' `shouldBe` solution

    it "throws CaptchaIdAlreadyExists when adding a CaptchaId twice" $ \connPool -> do
        Right () <- runVoidedQuery connPool $ storeCaptcha cid solution
        Left err <- runVoidedQuery connPool $ storeCaptcha cid "other text"
        err `shouldBe` CaptchaIdAlreadyExists

  where
    cid      = "RandomId"
    solution = "some text"


solveCaptchaSpec :: SpecWith (Pool Connection)
solveCaptchaSpec = describe "solveCaptcha" $ do
    it "returns true entry if the solution is correct" $ \connPool -> do
        Right () <- runVoidedQuery connPool $ storeCaptcha cid solution
        Right res <- runVoidedQuery connPool $ solveCaptcha cid solution
        res `shouldBe` True

    it "returns false entry if the solution is wrong" $ \connPool -> do
        Right () <- runVoidedQuery connPool $ storeCaptcha cid solution
        Right res <- runVoidedQuery connPool $ solveCaptcha cid "wrong text"
        res `shouldBe` False

    it "throws NoSuchCaptchaId if the given CaptchaId doesn't exist" $ \connPool -> do
        Left err <- runVoidedQuery connPool $ solveCaptcha cid solution
        err `shouldBe` NoSuchCaptchaId

  where
    cid      = "RandomId"
    solution = "some text"


deleteCaptchaSpec :: SpecWith (Pool Connection)
deleteCaptchaSpec = describe "deleteCaptcha" $ do
    it "deletes entry" $ \connPool -> do
        Right () <- runVoidedQuery connPool $ storeCaptcha cid solution
        rowCountShouldBe connPool "captchas" 1
        Right () <- runVoidedQuery connPool $ deleteCaptcha cid
        rowCountShouldBe connPool "captchas" 0

    it "throws NoSuchCaptchaId if the given CaptchaId doesn't exist" $ \connPool -> do
        Left err <- runVoidedQuery connPool $ deleteCaptcha cid
        err `shouldBe` NoSuchCaptchaId

    it "throws NoSuchCaptchaId if trying to delete the same captcha twice" $ \connPool -> do
        Right () <- runVoidedQuery connPool $ storeCaptcha cid solution
        Right () <- runVoidedQuery connPool $ deleteCaptcha cid
        Left err <- runVoidedQuery connPool $ deleteCaptcha cid
        err `shouldBe` NoSuchCaptchaId

  where
    cid      = "RandomId"
    solution = "some text"


-- * Garbage collection

garbageCollectUnconfirmedUsersSpec :: SpecWith (Pool Connection)
garbageCollectUnconfirmedUsersSpec = describe "garbageCollectUnconfirmedUsers" $ do
    let user1   = mkUser "name1" "pass" "email1@example.com"
        token1  = "sometoken1"
        user2   = mkUser "name2" "pass" "email2@example.com"
        token2  = "sometoken2"

    it "deletes all expired unconfirmed users" $ \connPool -> do
        Right _  <- runVoidedQuery connPool $ addUnconfirmedUser token1 user1
        Right _  <- runVoidedQuery connPool $ addUnconfirmedUser token2 user2
        Right () <- runVoidedQuery connPool $ garbageCollectUnconfirmedUsers $ fromSeconds 0
        rowCountShouldBe connPool "user_confirmation_tokens" 0
        rowCountShouldBe connPool "users" 0

    it "only deletes expired unconfirmed users" $ \connPool -> do
        Right _  <- runVoidedQuery connPool $ addUnconfirmedUser token1 user1
        Right () <- runVoidedQuery connPool $ garbageCollectUnconfirmedUsers $ fromHours 1
        rowCountShouldBe connPool "user_confirmation_tokens" 1
        rowCountShouldBe connPool "users" 1

garbageCollectPasswordResetTokensSpec :: SpecWith (Pool Connection)
garbageCollectPasswordResetTokensSpec = describe "garbageCollectPasswordResetTokens" $ do
    let user   = mkUser "name1" "pass" "email1@example.com"
        userId = UserId 321
        email = forceUserEmail "email1@example.com"
        passToken = "sometoken2"

    it "deletes all expired tokens" $ \connPool -> do
        void $ runVoidedQuery connPool $ addUserPrim (Just userId) user True
        void $ runVoidedQuery connPool $ addPasswordResetToken email passToken
        rowCountShouldBe connPool "password_reset_tokens" 1
        Right () <- runVoidedQuery connPool $ garbageCollectPasswordResetTokens $ fromSeconds 0
        rowCountShouldBe connPool "password_reset_tokens" 0

    it "only deletes expired tokens" $ \connPool -> do
        void $ runVoidedQuery connPool $ addUserPrim (Just userId) user True
        void $ runVoidedQuery connPool $ addPasswordResetToken email passToken
        void $ runVoidedQuery connPool $ garbageCollectPasswordResetTokens $ fromHours 1
        rowCountShouldBe connPool "password_reset_tokens" 1

garbageCollectEmailChangeTokensSpec :: SpecWith (Pool Connection)
garbageCollectEmailChangeTokensSpec = describe "garbageCollectEmailChangeTokens" $ do
    let newEmail = forceUserEmail "new@example.com"
        token = "sometoken2"

    it "deletes all expired tokens" $ \connPool -> do
        Right _ <- runVoidedQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runVoidedQuery connPool $ addUserEmailChangeRequest testUid newEmail token
        rowCountShouldBe connPool "email_change_tokens" 1
        Right () <- runVoidedQuery connPool $ garbageCollectEmailChangeTokens $ fromSeconds 0
        rowCountShouldBe connPool "email_change_tokens" 0

    it "only deletes expired tokens" $ \connPool -> do
        Right _ <- runVoidedQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runVoidedQuery connPool $ addUserEmailChangeRequest testUid newEmail token
        Right () <- runVoidedQuery connPool $ garbageCollectEmailChangeTokens $ fromHours 1
        rowCountShouldBe connPool "email_change_tokens" 1

garbageCollectThentosSessionsSpec :: SpecWith (Pool Connection)
garbageCollectThentosSessionsSpec = describe "garbageCollectThentosSessions" $ do
    it "deletes all expired thentos sessions" $ \connPool -> do
        let immediateTimeout = fromSeconds 0
        Right _ <- runVoidedQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runVoidedQuery connPool $ startThentosSession token (UserA testUid) immediateTimeout
        Right () <- runVoidedQuery connPool garbageCollectThentosSessions
        rowCountShouldBe connPool "thentos_sessions" 0

    it "doesn't delete active sessions" $ \connPool -> do
        let timeout = fromMinutes 1
        Right _ <- runVoidedQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runVoidedQuery connPool $ startThentosSession token (UserA testUid) timeout
        Right () <- runVoidedQuery connPool garbageCollectThentosSessions
        rowCountShouldBe connPool "thentos_sessions" 1

  where
    token = "thentos session token"

garbageCollectServiceSessionsSpec :: SpecWith (Pool Connection)
garbageCollectServiceSessionsSpec = describe "garbageCollectServiceSessions" $ do
    it "deletes (only) expired service sessions" $ \connPool -> do
        Right _ <- runVoidedQuery connPool $ addUserPrim (Just testUid) testUser True
        hashedKey <- hashServiceKey "secret"
        Right _ <- runVoidedQuery connPool $
            addService testUid sid hashedKey "sName" "sDescription"

        Right _ <- runVoidedQuery connPool $ startThentosSession tTok1 (UserA testUid) laterTimeout
        Right _ <- runVoidedQuery connPool $ startThentosSession tTok2 (UserA testUid) laterTimeout
        Right () <- runVoidedQuery connPool $ startServiceSession tTok1 sTok1 sid laterTimeout
        Right () <- runVoidedQuery connPool $ startServiceSession tTok2 sTok2 sid immediateTimeout
        Right () <- runVoidedQuery connPool garbageCollectServiceSessions

        [Only tok] <- doQuery_ connPool [sql| SELECT token FROM service_sessions |]
        tok `shouldBe` sTok1
        return ()
  where
    sid = "sid"
    tTok1 = "thentos token 1"
    tTok2 = "thentos token 2"
    sTok1 = "service token 1"
    sTok2 = "service token 2"
    immediateTimeout = fromSeconds 0
    laterTimeout = fromMinutes 1

garbageCollectCaptchasSpec :: SpecWith (Pool Connection)
garbageCollectCaptchasSpec = describe "garbageCollectCaptchas" $ do
    it "deletes all expired captchas" $ \connPool -> do
        Right () <- runVoidedQuery connPool $ storeCaptcha "cid-1" "solution-1"
        Right () <- runVoidedQuery connPool $ storeCaptcha "cid-2" "solution-2"
        rowCountShouldBe connPool "captchas" 2
        Right () <- runVoidedQuery connPool $ garbageCollectCaptchas $ fromSeconds 0
        rowCountShouldBe connPool "captchas" 0

    it "only deletes expired captchas" $ \connPool -> do
        Right () <- runVoidedQuery connPool $ storeCaptcha "cid-1" "solution-1"
        Right () <- runVoidedQuery connPool $ garbageCollectCaptchas $ fromHours 1
        rowCountShouldBe connPool "captchas" 1


-- * Utils

mkUser :: UserName -> UserPass -> ST -> User
mkUser name pass email = User { _userName = name
                              , _userPassword = encryptTestSecret fromUserPass pass
                              , _userEmail = forceUserEmail email
                              }

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Thentos.TransactionSpec (spec) where

import qualified Data.Set as Set
import Control.Lens ((^.))
import Control.Monad (void)
import Data.Either (isRight)
import Data.List (sort)
import Data.Pool (Pool)
import Data.String.Conversions (ST, SBS)
import Database.PostgreSQL.Simple (Connection, Only(..), Query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Test.Hspec (Spec, SpecWith, before, describe, it, pendingWith, shouldBe, shouldReturn,
                   shouldSatisfy)

import Thentos.Transaction
import Thentos.Types
import Thentos.Util (hashUserPass, hashServiceKey)

import Thentos.Test.Core
import Thentos.Test.Config
import Thentos.Test.Transaction

spec :: Spec
spec = describe "Thentos.Transaction" . before (createDb "test_thentos")
  $ do
    addUserPrimSpec
    addUserSpec
    addUnconfirmedUserSpec
    addUnconfirmedUserWithIdSpec
    finishUserRegistrationSpec
    finishUserRegistrationByIdSpec
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
    garbageCollectUnconfirmedUsersSpec
    garbageCollectPasswordResetTokensSpec
    garbageCollectEmailChangeTokensSpec
    garbageCollectThentosSessionsSpec
    garbageCollectServiceSessionsSpec
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
        void $ runQuery connPool $ addUserPrim (Just userId) user True
        Right (_, res) <- runQuery connPool $ lookupConfirmedUser userId
        res `shouldBe` user

    it "adds an unconfirmed user to the database" $ \connPool -> do
        let user   = testUsers !! 2
            userId = UserId 290
        void $ runQuery connPool $ addUserPrim (Just userId) user False
        runQuery connPool (lookupConfirmedUser userId) `shouldReturn` Left NoSuchUser
        Right (_, res) <- runQuery connPool $ lookupAnyUser userId
        res `shouldBe` user

    it "fails if the id is not unique" $ \connPool -> do
        let userId = UserId 289
        void $ runQuery connPool $ addUserPrim (Just userId) (testUsers !! 2) True
        x <- runQuery connPool $ addUserPrim (Just userId) (testUsers !! 3) True
        x `shouldBe` Left UserIdAlreadyExists

    it "fails if the username is not unique" $ \connPool -> do
        let user1 = mkUser "name" "pass1" "email1@example.com"
            user2 = mkUser "name" "pass2" "email2@example.com"
        void $ runQuery connPool $ addUserPrim (Just $ UserId 372) user1 True
        x <- runQuery connPool $ addUserPrim (Just $ UserId 482) user2 True
        x `shouldBe` Left UserNameAlreadyExists

    it "fails if the email is not unique" $  \connPool -> do
        let user1 = mkUser "name1" "pass1" "email@example.com"
            user2 = mkUser "name2" "pass2" "email@example.com"
        void $ runQuery connPool $ addUserPrim (Just $ UserId 372) user1 True
        x <- runQuery connPool $ addUserPrim (Just $ UserId 482) user2 True
        x `shouldBe` Left UserEmailAlreadyExists

addUserSpec :: SpecWith (Pool Connection)
addUserSpec = describe "addUser" $ do

    it "adds a user to the database" $ \connPool -> do
        void $ runQuery connPool $ mapM_ addUser testUsers
        let names = _userName <$> testUsers
        Right res <- runQuery connPool $ mapM lookupConfirmedUserByName names
        (snd <$> res) `shouldBe` testUsers

addUnconfirmedUserSpec :: SpecWith (Pool Connection)
addUnconfirmedUserSpec = describe "addUnconfirmedUser" $ do
    let user  = mkUser "name" "pass" "email@example.com"
        token = "sometoken"

    it "adds an unconfirmed a user to the database" $ \connPool -> do
        Right uid <- runQuery connPool $ addUnconfirmedUser token user
        runQuery connPool (lookupConfirmedUser uid) `shouldReturn` Left NoSuchUser
        Right (_, usr) <- runQuery connPool $ lookupAnyUser uid
        usr `shouldBe` user

addUnconfirmedUserWithIdSpec :: SpecWith (Pool Connection)
addUnconfirmedUserWithIdSpec = describe "addUnconfirmedUserWithId" $ do
    let user   = mkUser "name" "pass" "email@example.com"
        userId = UserId 321
        token  = "sometoken"

    it "adds an unconfirmed user to the DB" $ \connPool -> do
        Right () <- runQuery connPool $ addUnconfirmedUserWithId token user userId
        runQuery connPool (lookupConfirmedUser userId) `shouldReturn` Left NoSuchUser
        Right (_, usr) <- runQuery connPool . lookupAnyUserByEmail $
            forceUserEmail "email@example.com"
        usr `shouldBe` user

    it "adds the token for the user to the DB" $ \connPool -> do
        Right () <- runQuery connPool $ addUnconfirmedUserWithId token user userId
        [Only res] <- doQuery connPool [sql|
            SELECT token FROM user_confirmation_tokens
            WHERE id = ? |] (Only userId)
        res `shouldBe` token

    it "fails if the token is not unique" $ \connPool -> do
        let user2 = mkUser "name2" "pass" "email2@example.com"
            userId2 = UserId 322
        Right () <- runQuery connPool $ addUnconfirmedUserWithId token user userId
        Left err <- runQuery connPool $ addUnconfirmedUserWithId token user2 userId2
        err `shouldBe` ConfirmationTokenAlreadyExists

finishUserRegistrationByIdSpec :: SpecWith (Pool Connection)
finishUserRegistrationByIdSpec = describe "finishUserRegistrationById" $ do
    let user   = mkUser "name" "pass" "email@example.com"
        userId = UserId 321
        token  = "sometoken"

    it "makes the user be confirmed" $ \connPool -> do
        Right () <- runQuery connPool $ addUnconfirmedUserWithId token user userId
        [Only res1] <- doQuery connPool [sql|
            SELECT confirmed FROM "users"
            WHERE id = ? |] (Only userId)
        res1 `shouldBe` False
        Right () <- runQuery connPool $ finishUserRegistrationById userId
        [Only res2] <- doQuery connPool [sql|
            SELECT confirmed FROM "users"
            WHERE id = ? |] (Only userId)
        res2 `shouldBe` True

    it "removes the confirmation token" $ \connPool -> do
        Right () <- runQuery connPool $ addUnconfirmedUserWithId token user userId
        Right () <- runQuery connPool $ finishUserRegistrationById userId
        res <- doQuery connPool [sql|
            SELECT token FROM user_confirmation_tokens
            WHERE id = ? |] (Only userId)
        res `shouldBe` ([] :: [Only ConfirmationToken])

    it "fails if the user is already confirmed" $ \connPool -> do
        Right () <- runQuery connPool $ addUnconfirmedUserWithId token user userId
        Right () <- runQuery connPool $ finishUserRegistrationById userId
        Left err <- runQuery connPool $ finishUserRegistrationById userId
        err `shouldBe` NoSuchPendingUserConfirmation

    it "fails if the user doesn't exist" $ \connPool -> do
        Left err <- runQuery connPool $ finishUserRegistrationById userId
        err `shouldBe` NoSuchPendingUserConfirmation

finishUserRegistrationSpec :: SpecWith (Pool Connection)
finishUserRegistrationSpec = describe "finishUserRegistration" $ do
    it "confirms the user if the given token exists" $ \connPool -> do
        Right uid <- runQuery connPool $ addUnconfirmedUser token testUser
        Right uid' <- runQuery connPool $ finishUserRegistration timeout token
        uid `shouldBe` uid'
        [Only confirmed] <- doQuery connPool
            [sql| SELECT confirmed FROM users WHERE id = ? |] (Only uid)
        confirmed `shouldBe` True
        [Only tokenCount] <- doQuery_ connPool
            [sql| SELECT COUNT(*) FROM user_confirmation_tokens |]
        tokenCount `shouldBe` (0 :: Int)

    it "fails if the given token does not exist" $ \connPool -> do
        Right uid <- runQuery connPool $ addUnconfirmedUser token testUser
        Left NoSuchToken <- runQuery connPool $ finishUserRegistration timeout "badToken"
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
        void $ runQuery connPool $ addUserPrim (Just userId) user True
        runQuery connPool (lookupConfirmedUserByName "name") `shouldReturn` Right (userId, user)

    it "returns NoSuchUser for unconfirmed users" $ \connPool -> do
        let user = mkUser "name" "pass" "email@example.com"
            userId = UserId 437
        void $ runQuery connPool $ addUserPrim (Just userId) user False
        runQuery connPool (lookupConfirmedUserByName "name") `shouldReturn` Left NoSuchUser

    it "returns NoSuchUser if no user has the name" $ \connPool -> do
        runQuery connPool (lookupConfirmedUserByName "name") `shouldReturn` Left NoSuchUser

lookupConfirmedUserByEmailSpec :: SpecWith (Pool Connection)
lookupConfirmedUserByEmailSpec = describe "lookupConfirmedUserByEmail" $ do

    it "returns a confirmed user" $ \connPool -> do
        let user = mkUser "name" "pass" "email@example.com"
            userId = UserId 437
        void $ runQuery connPool $ addUserPrim (Just userId) user True
        runQuery connPool (lookupConfirmedUserByEmail $ forceUserEmail "email@example.com")
            `shouldReturn` Right (userId, user)

    it "returns NoSuchUser for unconfirmed users" $ \connPool -> do
        let user = mkUser "name" "pass" "email@example.com"
            userId = UserId 437
        void $ runQuery connPool $ addUserPrim (Just userId) user False
        runQuery connPool (lookupConfirmedUserByEmail $ forceUserEmail "email@example.com")
            `shouldReturn` Left NoSuchUser

    it "returns NoSuchUser if no user has the email" $ \connPool -> do
        runQuery connPool (lookupConfirmedUserByEmail $ forceUserEmail "email@example.com")
            `shouldReturn` Left NoSuchUser

lookupAnyUserByEmailSpec :: SpecWith (Pool Connection)
lookupAnyUserByEmailSpec = describe "lookupAnyUserByEmail" $ do

    it "returns a confirmed user" $ \connPool -> do
        let user = mkUser "name" "pass" "email@example.com"
            userId = UserId 437
        void $ runQuery connPool $ addUserPrim (Just userId) user True
        runQuery connPool (lookupAnyUserByEmail $ forceUserEmail "email@example.com")
            `shouldReturn` Right (userId, user)

    it "returns an unconfirmed user" $ \connPool -> do
        let user = mkUser "name" "pass" "email@example.com"
            userId = UserId 437
        void $ runQuery connPool $ addUserPrim (Just userId) user False
        runQuery connPool (lookupAnyUserByEmail $ forceUserEmail "email@example.com")
            `shouldReturn` Right (userId, user)

    it "returns NoSuchUser if no user has the email" $ \connPool -> do
        runQuery connPool (lookupAnyUserByEmail $ forceUserEmail "email@example.com")
            `shouldReturn` Left NoSuchUser

deleteUserSpec :: SpecWith (Pool Connection)
deleteUserSpec = describe "deleteUser" $ do

    it "deletes a user" $ \connPool -> do
        let user = mkUser "name" "pass" "email@example.com"
            userId = UserId 371
        void $ runQuery connPool $ addUserPrim (Just userId) user True
        Right _  <- runQuery connPool $ lookupAnyUser userId
        Right () <- runQuery connPool $ deleteUser userId
        runQuery connPool (lookupAnyUser userId) `shouldReturn` Left NoSuchUser

    it "throws NoSuchUser if the id does not exist" $ \connPool -> do
        runQuery connPool (deleteUser $ UserId 210) `shouldReturn` Left NoSuchUser

passwordResetTokenSpec :: SpecWith (Pool Connection)
passwordResetTokenSpec = describe "addPasswordResetToken" $ do
    it "adds a password reset to the db" $ \connPool -> do
        let user = mkUser "name" "super secret" "me@example.com"
            userId = UserId 584
            testToken = PasswordResetToken "asgbagbaosubgoas"
        Right _ <- runQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runQuery connPool $
            addPasswordResetToken (user ^. userEmail) testToken
        [Only token_in_db] <- doQuery connPool
            [sql| SELECT token FROM password_reset_tokens |] ()
        token_in_db `shouldBe` testToken

    it "resets a password if the given token exists" $ \connPool -> do
        let user = mkUser "name" "super secret" "me@example.com"
            userId = UserId 594
            testToken = PasswordResetToken "asgbagbaosubgoas"
        newEncryptedPass <- hashUserPass "newSecretP4ssw0rd"
        Right _ <- runQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runQuery connPool $
            addPasswordResetToken (user ^. userEmail) testToken
        Right _ <- runQuery connPool $
            resetPassword (fromHours 1) testToken newEncryptedPass
        [Only newPassInDB] <- doQuery connPool
            [sql| SELECT password FROM users WHERE id = ?|] (Only userId)
        newPassInDB `shouldBe` newEncryptedPass

changePasswordSpec :: SpecWith (Pool Connection)
changePasswordSpec = describe "changePassword" $ do
    let user = mkUser "name" "super secret" "me@example.com"
        userId = UserId 111
        newPass = encryptTestSecret "new"

    it "changes the password" $ \connPool -> do
        Right _ <- runQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runQuery connPool $ changePassword userId newPass
        Right (_, usr) <- runQuery connPool $ lookupConfirmedUser userId
        _userPassword usr `shouldBe` newPass

    it "fails if the user doesn't exist" $ \connPool -> do
        Left err <- runQuery connPool $ changePassword userId newPass
        err `shouldBe` NoSuchUser


agentRolesSpec :: SpecWith (Pool Connection)
agentRolesSpec = describe "agentRoles" $ do
    it "returns an empty set for a user or service without roles" $
      \connPool -> do
        Right _ <- runQuery connPool $ addUserPrim (Just testUid) testUser True
        x <- runQuery connPool $ agentRoles (UserA testUid)
        x `shouldBe` Right []

        Right _ <- runQuery connPool $
            addService (UserA testUid) sid testHashedSecret "name" "desc"
        roles <- runQuery connPool $ agentRoles (ServiceA sid)
        roles `shouldBe` Right []
  where
    sid = "sid"

assignRoleSpec :: SpecWith (Pool Connection)
assignRoleSpec = describe "assignRole" $ do
    it "adds a role" $ \connPool -> do
        Right _ <- runQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runQuery connPool $ assignRole (UserA testUid) RoleAdmin
        Right roles <- runQuery connPool $ agentRoles (UserA testUid)
        roles `shouldBe` [RoleAdmin]

        addTestService connPool
        Right _ <- runQuery connPool $ assignRole (ServiceA sid) RoleAdmin
        Right serviceRoles <- runQuery connPool $ agentRoles (ServiceA sid)
        serviceRoles `shouldBe` [RoleAdmin]

    it "silently allows adding a duplicate role" $ \connPool -> do
        Right _ <- runQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runQuery connPool $ assignRole (UserA testUid) RoleAdmin
        x <- runQuery connPool $ assignRole (UserA testUid) RoleAdmin
        x `shouldBe` Right ()
        Right roles <- runQuery connPool $ agentRoles (UserA testUid)
        roles `shouldBe` [RoleAdmin]

        addTestService connPool
        Right () <- runQuery connPool $ assignRole (ServiceA sid) RoleAdmin
        Right () <- runQuery connPool $ assignRole (ServiceA sid) RoleAdmin
        Right serviceRoles <- runQuery connPool $ agentRoles (ServiceA sid)
        serviceRoles `shouldBe` [RoleAdmin]

    it "adds a second role" $ \connPool -> do
        Right _ <- runQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runQuery connPool $ assignRole (UserA testUid) RoleAdmin
        Right _ <- runQuery connPool $ assignRole (UserA testUid) RoleUser
        Right roles <- runQuery connPool $ agentRoles (UserA testUid)
        Set.fromList roles `shouldBe` Set.fromList [RoleAdmin, RoleUser]

        addTestService connPool
        Right _ <- runQuery connPool $ assignRole (ServiceA sid) RoleAdmin
        Right _ <- runQuery connPool $ assignRole (ServiceA sid) RoleUser
        Right serviceRoles <- runQuery connPool $ agentRoles (ServiceA sid)
        Set.fromList serviceRoles `shouldBe` Set.fromList [RoleAdmin, RoleUser]

  where
    sid = "sid"
    addTestService connPool = void . runQuery connPool $
        addService (UserA testUid) sid testHashedSecret "name" "desc"

unassignRoleSpec :: SpecWith (Pool Connection)
unassignRoleSpec = describe "unassignRole" $ do
    it "silently allows removing a non-assigned role" $ \connPool -> do
        Right _ <- runQuery connPool $ addUserPrim (Just testUid) testUser True
        x <- runQuery connPool $ unassignRole (UserA testUid) RoleAdmin
        x `shouldBe` Right ()

        addTestService connPool
        res <- runQuery connPool $ unassignRole (ServiceA sid) RoleAdmin
        res `shouldBe` Right ()

    it "removes the specified role" $ \connPool -> do
        Right _ <- runQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runQuery connPool $ assignRole (UserA testUid) RoleAdmin
        Right _ <- runQuery connPool $ assignRole (UserA testUid) RoleUser
        Right _ <- runQuery connPool $ unassignRole (UserA testUid) RoleAdmin
        Right roles <- runQuery connPool $ agentRoles (UserA testUid)
        roles `shouldBe` [RoleUser]

        addTestService connPool
        Right _ <- runQuery connPool $ assignRole (ServiceA sid) RoleAdmin
        Right _ <- runQuery connPool $ assignRole (ServiceA sid) RoleUser
        Right _ <- runQuery connPool $ unassignRole (ServiceA sid) RoleAdmin
        Right serviceRoles <- runQuery connPool $ agentRoles (ServiceA sid)
        serviceRoles `shouldBe` [RoleUser]

  where
    sid = "sid"
    addTestService connPool = void . runQuery connPool $
        addService (UserA testUid) sid testHashedSecret "name" "desc"

emailChangeRequestSpec :: SpecWith (Pool Connection)
emailChangeRequestSpec = describe "addUserEmailChangeToken" $ do
    it "adds an email change token to the db" $ \connPool -> do
        Right _ <- runThentosQueryFromPool connPool $ addUserPrim (Just userId) user True
        Right _ <- runThentosQueryFromPool connPool $
            addUserEmailChangeRequest userId newEmail testToken
        [Only tokenInDb] <- doQuery connPool
            [sql| SELECT token FROM email_change_tokens|] ()
        tokenInDb `shouldBe` testToken

    it "changes a user's email if given a valid token" $ \connPool -> do
        Right _ <- runThentosQueryFromPool connPool $ addUserPrim (Just userId) user True
        Right _ <- runThentosQueryFromPool connPool $
            addUserEmailChangeRequest userId newEmail testToken
        Right _ <-
            runThentosQueryFromPool connPool $ confirmUserEmailChange (fromHours 1) testToken
        [Only expectedEmail] <- doQuery connPool
            [sql| SELECT email FROM users WHERE id = ?|] (Only userId)
        expectedEmail `shouldBe` newEmail

    it "throws NoSuchToken if given an invalid token and does not update the email" $ \connPool -> do
        Right _ <- runThentosQueryFromPool connPool $ addUserPrim (Just userId) user True
        Right _ <- runThentosQueryFromPool connPool $
            addUserEmailChangeRequest userId newEmail testToken
        let badToken = ConfirmationToken "badtoken"
        Left NoSuchToken <-
            runThentosQueryFromPool connPool $ confirmUserEmailChange (Timeoutms 3600) badToken
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
        Right _ <- runThentosQueryFromPool connPool $ addUserPrim (Just uid) user True
        [Only serviceCount] <- doQuery connPool countServices ()
        serviceCount `shouldBe` (0 :: Int)
        Right _ <- runThentosQueryFromPool connPool $
            addService (UserA uid) sid testHashedSecret name description
        [(owner', sid', key', name', desc')] <- doQuery connPool
            [sql| SELECT owner_user, id, key, name, description
                  FROM services |] ()
        owner' `shouldBe` uid
        sid' `shouldBe` sid
        key' `shouldBe` testHashedSecret
        name' `shouldBe` name
        desc' `shouldBe` description

    it "allows a service's owner to be a service" $ \connPool -> do
        let childSid = "child_sid"
            childName = "child service"
        Right _ <- runThentosQueryFromPool connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runThentosQueryFromPool connPool $
            addService (UserA testUid) sid testHashedSecret name description
        Right _ <- runThentosQueryFromPool connPool $
            addService (ServiceA sid) childSid testHashedSecret childName "foo"

        [Only serviceCount] <- doQuery connPool countServices ()
        serviceCount `shouldBe` (2 :: Int)

        [(sid', name')] <- doQuery connPool
            [sql| SELECT id, name
                  FROM services
                  WHERE owner_service = ? |] (Only sid)
        sid' `shouldBe` childSid
        name' `shouldBe` childName

  where
    sid = ServiceId "serviceid1"
    uid = UserId 9
    user = mkUser "name" "super secret" "me@example.com"
    name = ServiceName "MyLittleService"
    description = ServiceDescription "it serves"
    countServices = [sql| SELECT COUNT(*) FROM services |]

deleteServiceSpec :: SpecWith (Pool Connection)
deleteServiceSpec = describe "deleteService" $ do
    it "deletes a service" $ \connPool -> do
        Right _ <- runThentosQueryFromPool connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runThentosQueryFromPool connPool $
            addService (UserA testUid) sid testHashedSecret "" ""
        Right _ <- runThentosQueryFromPool connPool $ deleteService sid
        [Only serviceCount] <- doQuery connPool [sql| SELECT COUNT(*) FROM services |] ()
        serviceCount `shouldBe` (0 :: Int)

    it "fails if the service doesn't exist" $ \connPool -> do
        Left NoSuchService <- runThentosQueryFromPool connPool $ deleteService sid
        return ()
  where
    sid = ServiceId "blablabla"

lookupServiceSpec :: SpecWith (Pool Connection)
lookupServiceSpec = describe "lookupService" $ do
    it "looks up a service"  $ \connPool -> do
        Right _ <- runThentosQueryFromPool connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runThentosQueryFromPool connPool $
            addService (UserA testUid) sid testHashedSecret "name" "desc"

        Right (sid', service) <- runThentosQueryFromPool connPool $ lookupService sid
        service ^. serviceKey `shouldBe` testHashedSecret
        sid' `shouldBe` sid
        service ^. serviceName `shouldBe` name
        service ^. serviceDescription `shouldBe` desc
        service ^. serviceOwner `shouldBe` UserA testUid
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
        void $ runQuery connPool $ addUserPrim (Just testUid) testUser True
        Right () <- runQuery connPool $ startThentosSession tok (UserA testUid) period
        [Only uid] <- doQuery connPool [sql| SELECT uid
                                             FROM thentos_sessions
                                             WHERE token = ? |] (Only tok)
        uid `shouldBe` testUid

    it "fails if the user is uconfirmed" $ \connPool -> do
        void $ runQuery connPool $ addUserPrim (Just testUid) testUser False
        x <- runQuery connPool $ startThentosSession tok user period
        x `shouldBe` Left NoSuchUser

    it "fails if the user doesn't exist" $ \connPool -> do
        x <- runQuery connPool $ startThentosSession tok user period
        x `shouldBe` Left NoSuchUser

    it "creates a thentos session for a service" $ \connPool -> do
        let sid = "sid"
        void $ runQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runThentosQueryFromPool connPool $
            addService (UserA testUid) sid testHashedSecret "name" "desc"
        Right () <- runQuery connPool $ startThentosSession tok (ServiceA sid) period
        [Only sid'] <- doQuery connPool [sql| SELECT sid
                                         FROM thentos_sessions
                                         WHERE token = ? |] (Only tok)
        sid' `shouldBe` sid

    it "fails if the service doesn't exist" $ \connPool -> do
        x <- runQuery connPool $ startThentosSession tok (ServiceA "sid") period
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
        void $ runQuery connPool $ addUserPrim (Just userId) user True
        x <- runQuery connPool $ lookupThentosSession tok
        x `shouldBe` Left NoSuchThentosSession

    it "reads back a fresh session" $ \connPool -> do
        void $ runQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runQuery connPool $ startThentosSession tok agent period
        Right (t, s) <- runQuery connPool $ lookupThentosSession tok
        t `shouldBe` tok
        s ^. thSessAgent `shouldBe` agent

        let tok2 = "anothertoken"
        Right _ <- runThentosQueryFromPool connPool $
            addService (UserA userId) sid testHashedSecret "name" "desc"
        Right _ <- runQuery connPool $ startThentosSession tok2 (ServiceA sid) period
        Right (tok2', sess) <- runQuery connPool $ lookupThentosSession tok2
        tok2' `shouldBe` tok2
        sess ^. thSessAgent `shouldBe` ServiceA sid

    it "fails for an expired session" $ \connPool -> do
        void $ runQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runQuery connPool $ startThentosSession tok agent (fromSeconds 0)
        x <- runQuery connPool $ lookupThentosSession tok
        x `shouldBe` Left NoSuchThentosSession

    it "extends the session" $ \connPool -> do
        void $ runQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runQuery connPool $ startThentosSession tok agent period
        Right (_, sess1) <- runQuery connPool $ lookupThentosSession tok
        Right (t, sess2) <- runQuery connPool $ lookupThentosSession tok
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
        void $ runQuery connPool $ addUserPrim (Just userId) user True
        void $ runQuery connPool $ startThentosSession tok agent period
        Right _ <- runQuery connPool $ lookupThentosSession tok
        Right _ <- runQuery connPool $ endThentosSession tok
        x <- runQuery connPool $ lookupThentosSession tok
        x `shouldBe` Left NoSuchThentosSession

    it "silently allows deleting a non-existing session" $ \connPool -> do
        x <- runQuery connPool $ endThentosSession tok
        x `shouldSatisfy` isRight

serviceNamesFromThentosSessionSpec :: SpecWith (Pool Connection)
serviceNamesFromThentosSessionSpec = describe "serviceNamesFromThentosSession" $ do
    it "gets the names of all services that a thentos session is signed into" $ \conn -> do
        let go = void . runQuery conn
        go $ addUserPrim (Just testUid) testUser True
        go $ addService (UserA testUid) "sid1" testHashedSecret "s1-name" "s1-desc"
        go $ addService (UserA testUid) "sid2" testHashedSecret "s2-name" "s2-desc"
        go $ addService (UserA testUid) "sid3" testHashedSecret "s3-name" "s3-desc"
        go $ startThentosSession thentosSessionToken (UserA testUid) period
        go $ startServiceSession thentosSessionToken "sst1" "sid1" period
        go $ startServiceSession thentosSessionToken "sst2" "sid2" period
        Right names <- runQuery conn $ serviceNamesFromThentosSession thentosSessionToken
        sort names `shouldBe` ["s1-name", "s2-name"]
        return ()
  where
    thentosSessionToken = "abcde"
    period = fromMinutes 1

startServiceSessionSpec :: SpecWith (Pool Connection)
startServiceSessionSpec = describe "startServiceSession" $ do
    it "starts a service session" $ \connPool -> do
        void $ runQuery connPool $ addUserPrim (Just testUid) testUser True
        void $ runQuery connPool $
            startThentosSession thentosSessionToken (UserA testUid) period
        void $ runQuery connPool $
            addService (UserA testUid) sid testHashedSecret "" ""
        void $ runQuery connPool $
            startServiceSession thentosSessionToken serviceSessionToken sid period
        [Only count] <- doQuery_ connPool [sql| SELECT COUNT(*) FROM service_sessions |]
        count `shouldBe` (1 :: Int)
  where
    period = fromMinutes 1
    thentosSessionToken = "foo"
    serviceSessionToken = "bar"
    sid = "sid"

endServiceSessionSpec :: SpecWith (Pool Connection)
endServiceSessionSpec = describe "endServiceSession" $ do
    it "ends an service session" $ \connPool -> do
        void $ runQuery connPool $ addUserPrim (Just testUid) testUser True
        void $ runQuery connPool $
            startThentosSession thentosSessionToken (UserA testUid) period
        void $ runQuery connPool $
            addService (UserA testUid) sid testHashedSecret "" ""
        void $ runQuery connPool $
            startServiceSession thentosSessionToken serviceSessionToken sid period
        [Only count] <- doQuery_ connPool countSessions
        count `shouldBe` (1 :: Int)
        void $ runQuery connPool $ endServiceSession serviceSessionToken
        [Only count'] <- doQuery_ connPool countSessions
        count' `shouldBe` (0 :: Int)
        return ()
  where
    countSessions = [sql| SELECT COUNT(*) FROM service_sessions |]
    period = fromMinutes 1
    thentosSessionToken = "foo"
    serviceSessionToken = "bar"
    sid = "sid"

lookupServiceSessionSpec :: SpecWith (Pool Connection)
lookupServiceSessionSpec = describe "lookupServiceSession" $ do
    it "looks up the service session with a given token" $ \connPool -> do
        void $ runQuery connPool $ addUserPrim (Just testUid) testUser True
        void $ runQuery connPool $
            startThentosSession thentosSessionToken (UserA testUid) period
        void $ runQuery connPool $
            addService (UserA testUid) sid testHashedSecret "" ""
        void $ runQuery connPool $
            startServiceSession thentosSessionToken serviceSessionToken sid period
        Right (tok, sess) <- runQuery connPool $ lookupServiceSession serviceSessionToken
        sess ^. srvSessService `shouldBe` sid
        sess ^. srvSessExpirePeriod `shouldBe` period
        tok `shouldBe` serviceSessionToken

    it "returns NoSuchServiceSession error if no service with the given id exists" $
      \connPool -> do
        Left err <- runQuery connPool $ lookupServiceSession "non-existent token"
        err `shouldBe` NoSuchServiceSession

  where
    period = fromMinutes 1
    thentosSessionToken = "foo"
    serviceSessionToken = "bar"
    sid = "sid"


-- * persona and context

-- | Some helper SQL queries.
countContexts :: Query
countContexts = [sql| SELECT COUNT(*) FROM contexts |]

countPersonas :: Query
countPersonas = [sql| SELECT COUNT(*) FROM personas |]

addPersonaSpec :: SpecWith (Pool Connection)
addPersonaSpec = describe "addPersona" $ do
    it "adds a persona to the DB" $ \connPool -> do
        Right uid <- runQuery connPool $ addUser (head testUsers)
        [Only personaCount] <- doQuery connPool countPersonas ()
        personaCount `shouldBe` (0 :: Int)
        Right persona <- runThentosQueryFromPool connPool $ addPersona persName uid
        persona ^. personaName `shouldBe` persName
        persona ^. personaUid `shouldBe` uid
        [(id', name', uid')] <- doQuery connPool [sql| SELECT id, name, uid FROM personas |] ()
        id' `shouldBe` persona ^. personaId
        name' `shouldBe` persName
        uid' `shouldBe` uid

    it "throws NoSuchUser if the persona belongs to a non-existent user" $ \connPool -> do
        Left err <- runQuery connPool . addPersona persName $ UserId 6696
        err `shouldBe` NoSuchUser

    it "throws PersonaNameAlreadyExists if the name is not unique" $ \connPool -> do
        Right uid <- runQuery connPool $ addUser (head testUsers)
        Right _   <- runQuery connPool $ addPersona persName uid
        Left err  <- runQuery connPool $ addPersona persName uid
        err `shouldBe` PersonaNameAlreadyExists

deletePersonaSpec :: SpecWith (Pool Connection)
deletePersonaSpec = describe "deletePersona" $ do
    it "deletes a persona" $ \connPool -> do
        Right uid <- runQuery connPool $ addUser (head testUsers)
        [Only personaCount] <- doQuery connPool countPersonas ()
        personaCount `shouldBe` (0 :: Int)
        Right persona <- runThentosQueryFromPool connPool $ addPersona persName uid
        Right () <- runQuery connPool . deletePersona $ persona ^. personaId
        [Only personaCount'] <- doQuery connPool countPersonas ()
        personaCount' `shouldBe` (0 :: Int)

    it "throws NoSuchPersona if the persona does not exist" $ \connPool -> do
        Left err <- runQuery connPool . deletePersona $ PersonaId 5432
        err `shouldBe` NoSuchPersona

addContextSpec :: SpecWith (Pool Connection)
addContextSpec = describe "addContext" $ do
    it "adds a context to the DB" $ \connPool -> do
        Right uid <- runQuery connPool $ addUser (head testUsers)
        Right ()  <- runQuery connPool $
                        addService (UserA uid) servId testHashedSecret "sName" "sDescription"
        [Only contextCount] <- doQuery connPool countContexts ()
        contextCount `shouldBe` (0 :: Int)
        Right cxt <- runThentosQueryFromPool connPool $ addContext servId cxtName cxtDesc cxtUrl
        cxt ^. contextService `shouldBe` servId
        cxt ^. contextName `shouldBe` cxtName
        cxt ^. contextDescription `shouldBe` cxtDesc
        cxt ^. contextUrl `shouldBe` cxtUrl
        [(id', name, sid, desc, url)] <- doQuery connPool
            [sql| SELECT id, name, owner_service, description, url FROM contexts |] ()
        id' `shouldBe` cxt ^. contextId
        name `shouldBe` cxtName
        sid `shouldBe` servId
        desc `shouldBe` cxtDesc
        url `shouldBe` cxtUrl

    it "throws NoSuchService if the context belongs to a non-existent service" $ \connPool -> do
        Left err <- runQuery connPool $ addContext servId cxtName cxtDesc cxtUrl
        err `shouldBe` NoSuchService

    it "throws ContextNameAlreadyExists if the name is not unique" $ \connPool -> do
        Right uid <- runQuery connPool $ addUser (head testUsers)
        Right ()  <- runQuery connPool $
                        addService (UserA uid) servId testHashedSecret "sName" "sDescription"
        Right _   <- runQuery connPool $ addContext servId cxtName cxtDesc cxtUrl
        Left err  <- runQuery connPool $ addContext servId cxtName cxtDesc cxtUrl
        err `shouldBe` ContextNameAlreadyExists

deleteContextSpec :: SpecWith (Pool Connection)
deleteContextSpec = describe "deleteContext" $ do
    it "deletes a context from the DB" $ \connPool -> do
        Right uid <- runQuery connPool $ addUser (head testUsers)
        Right ()  <- runQuery connPool $
                        addService (UserA uid) servId testHashedSecret "sName" "sDescription"
        [Only contextCount] <- doQuery connPool countContexts ()
        contextCount `shouldBe` (0 :: Int)
        Right cxt <- runThentosQueryFromPool connPool $ addContext servId cxtName cxtDesc cxtUrl
        Right ()  <- runThentosQueryFromPool connPool . deleteContext $ cxt ^. contextId
        [Only contextCount'] <- doQuery connPool countContexts ()
        contextCount' `shouldBe` (0 :: Int)

    it "throws NoSuchContext if the context doesn't exist" $ \connPool -> do
        Left err <- runQuery connPool . deleteContext $ ContextId 1525
        err `shouldBe` NoSuchContext

registerPersonaWithContextSpec :: SpecWith (Pool Connection)
registerPersonaWithContextSpec = describe "registerPersonaWithContext" $ do
    it "connects a persona with a context" $ \connPool -> do
        Right uid     <- runQuery connPool $ addUser (head testUsers)
        Right persona <- runThentosQueryFromPool connPool $ addPersona persName uid
        Right ()      <- runQuery connPool $
                            addService (UserA uid) servId testHashedSecret "sName" "sDescription"
        Right cxt     <- runThentosQueryFromPool connPool $ addContext servId cxtName cxtDesc cxtUrl
        Right ()      <- runThentosQueryFromPool connPool . registerPersonaWithContext persona
                            $ cxt ^. contextId
        [(pid, cid)] <- doQuery connPool
                            [sql| SELECT persona_id, context_id FROM personas_per_context |] ()
        cid `shouldBe` cxt ^. contextId
        pid `shouldBe` persona ^. personaId

    it "throws MultiplePersonasPerContext if the persona is already registered" $ \connPool -> do
        Right uid     <- runQuery connPool $ addUser (head testUsers)
        Right persona <- runThentosQueryFromPool connPool $ addPersona persName uid
        Right ()      <- runQuery connPool $
                            addService (UserA uid) servId testHashedSecret "sName" "sDescription"
        Right cxt     <- runThentosQueryFromPool connPool $ addContext servId cxtName cxtDesc cxtUrl
        Right ()      <- runThentosQueryFromPool connPool . registerPersonaWithContext persona
                            $ cxt ^. contextId
        Left err      <- runQuery connPool . registerPersonaWithContext persona $ cxt ^. contextId
        err `shouldBe` MultiplePersonasPerContext

    it "throws MultiplePersonasPerContext if the user registered another persona" $ \connPool -> do
        Right uid      <- runQuery connPool $ addUser (head testUsers)
        Right persona  <- runThentosQueryFromPool connPool $ addPersona persName uid
        Right persona' <- runThentosQueryFromPool connPool $ addPersona "MyMyMy" uid
        Right ()  <- runQuery connPool $
                            addService (UserA uid) servId testHashedSecret "sName" "sDescription"
        Right cxt <- runThentosQueryFromPool connPool $ addContext servId cxtName cxtDesc cxtUrl
        Right ()  <- runThentosQueryFromPool connPool . registerPersonaWithContext persona
                            $ cxt ^. contextId
        Left err  <- runQuery connPool . registerPersonaWithContext persona' $ cxt ^. contextId
        err `shouldBe` MultiplePersonasPerContext

    it "throws NoSuchPersona if the persona doesn't exist" $ \connPool -> do
        Right uid <- runQuery connPool $ addUser (head testUsers)
        let persona = Persona (PersonaId 5904) persName uid
        Right ()  <- runQuery connPool $
                            addService (UserA uid) servId testHashedSecret "sName" "sDescription"
        Right cxt <- runThentosQueryFromPool connPool $ addContext servId cxtName cxtDesc cxtUrl
        Left err  <- runQuery connPool . registerPersonaWithContext persona $ cxt ^. contextId
        err `shouldBe` NoSuchPersona

    it "throws NoSuchContext if the context doesn't exist" $ \connPool -> do
        Right uid     <- runQuery connPool $ addUser (head testUsers)
        Right persona <- runThentosQueryFromPool connPool $ addPersona persName uid
        Right ()      <- runQuery connPool $
                            addService (UserA uid) servId testHashedSecret "sName" "sDescription"
        Left err  <- runQuery connPool . registerPersonaWithContext persona $ ContextId 1525
        err `shouldBe` NoSuchContext

unregisterPersonaFromContextSpec :: SpecWith (Pool Connection)
unregisterPersonaFromContextSpec = describe "unregisterPersonaFromContext" $ do
    it "TODO" $ \_connPool -> do
        pendingWith "not yet implemented"

findPersonaSpec :: SpecWith (Pool Connection)
findPersonaSpec = describe "findPersona" $ do
    it "TODO" $ \_connPool -> do
        pendingWith "not yet implemented"

contextsForServiceSpec :: SpecWith (Pool Connection)
contextsForServiceSpec = describe "contextsForService" $ do
    it "TODO" $ \_connPool -> do
        pendingWith "not yet implemented"


-- * Garbage collection

garbageCollectUnconfirmedUsersSpec :: SpecWith (Pool Connection)
garbageCollectUnconfirmedUsersSpec = describe "garbageCollectUnconfirmedUsers" $ do
    let user1   = mkUser "name1" "pass" "email1@example.com"
        userId1 = UserId 321
        token1  = "sometoken1"
        user2   = mkUser "name2" "pass" "email2@example.com"
        userId2 = UserId 322
        token2  = "sometoken2"

    it "deletes all expired unconfirmed users" $ \connPool -> do
        Right () <- runQuery connPool $ addUnconfirmedUserWithId token1 user1 userId1
        Right () <- runQuery connPool $ addUnconfirmedUserWithId token2 user2 userId2
        Right () <- runQuery connPool $ garbageCollectUnconfirmedUsers $ fromSeconds 0
        [Only tkns] <- doQuery_ connPool [sql| SELECT count(*) FROM user_confirmation_tokens |]
        [Only usrs] <- doQuery_ connPool [sql| SELECT count(*) FROM "users" |]
        tkns `shouldBe` (0 :: Int)
        usrs `shouldBe` (0 :: Int)

    it "only deletes expired unconfirmed users" $ \connPool -> do
        Right () <- runQuery connPool $ addUnconfirmedUserWithId token1 user1 userId1
        Right () <- runQuery connPool $ garbageCollectUnconfirmedUsers $ fromHours 1
        [Only tkns] <- doQuery_ connPool [sql| SELECT count(*) FROM user_confirmation_tokens |]
        [Only usrs] <- doQuery_ connPool [sql| SELECT count(*) FROM "users" |]
        tkns `shouldBe` (1 :: Int)
        usrs `shouldBe` (1 :: Int)

garbageCollectPasswordResetTokensSpec :: SpecWith (Pool Connection)
garbageCollectPasswordResetTokensSpec = describe "garbageCollectPasswordResetTokens" $ do
    let user   = mkUser "name1" "pass" "email1@example.com"
        userId = UserId 321
        email = forceUserEmail "email1@example.com"
        passToken = "sometoken2"

    it "deletes all expired tokens" $ \connPool -> do
        void $ runQuery connPool $ addUserPrim (Just userId) user True
        void $ runQuery connPool $ addPasswordResetToken email passToken
        [Only tkns] <- doQuery_ connPool [sql| SELECT count(*) FROM password_reset_tokens |]
        tkns `shouldBe` (1 :: Int)
        Right () <- runQuery connPool $ garbageCollectPasswordResetTokens $ fromSeconds 0
        [Only tkns'] <- doQuery_ connPool [sql| SELECT count(*) FROM password_reset_tokens |]
        tkns' `shouldBe` (0 :: Int)

    it "only deletes expired tokens" $ \connPool -> do
        void $ runQuery connPool $ addUserPrim (Just userId) user True
        void $ runQuery connPool $ addPasswordResetToken email passToken
        void $ runQuery connPool $ garbageCollectPasswordResetTokens $ fromHours 1
        [Only tkns'] <- doQuery_ connPool [sql| SELECT count(*) FROM password_reset_tokens |]
        tkns' `shouldBe` (1 :: Int)

garbageCollectEmailChangeTokensSpec :: SpecWith (Pool Connection)
garbageCollectEmailChangeTokensSpec = describe "garbageCollectEmailChangeTokens" $ do
    let newEmail = forceUserEmail "new@example.com"
        token = "sometoken2"

    it "deletes all expired tokens" $ \connPool -> do
        Right _ <- runQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runQuery connPool $ addUserEmailChangeRequest testUid newEmail token
        [Only tokenCount] <- doQuery_ connPool [sql| SELECT count(*) FROM email_change_tokens |]
        tokenCount `shouldBe` (1 :: Int)
        Right () <- runQuery connPool $ garbageCollectEmailChangeTokens $ fromSeconds 0
        [Only tokenCount'] <- doQuery_ connPool [sql| SELECT count(*) FROM email_change_tokens |]
        tokenCount' `shouldBe` (0 :: Int)

    it "only deletes expired tokens" $ \connPool -> do
        Right _ <- runQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runQuery connPool $ addUserEmailChangeRequest testUid newEmail token
        Right () <- runQuery connPool $ garbageCollectEmailChangeTokens $ fromHours 1
        [Only tkns'] <- doQuery_ connPool [sql| SELECT count(*) FROM email_change_tokens |]
        tkns' `shouldBe` (1 :: Int)

garbageCollectThentosSessionsSpec :: SpecWith (Pool Connection)
garbageCollectThentosSessionsSpec = describe "garbageCollectThentosSessions" $ do
    it "deletes all expired thentos sessions" $ \connPool -> do
        let immediateTimeout = fromSeconds 0
        Right _ <- runQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runQuery connPool $ startThentosSession token (UserA testUid) immediateTimeout
        Right () <- runQuery connPool garbageCollectThentosSessions
        [Only sessionCount] <- doQuery_ connPool [sql| SELECT count(*) FROM thentos_sessions |]
        sessionCount `shouldBe` (0 :: Int)

    it "doesn't delete active sessions" $ \connPool -> do
        let timeout = fromMinutes 1
        Right _ <- runQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runQuery connPool $ startThentosSession token (UserA testUid) timeout
        Right () <- runQuery connPool garbageCollectThentosSessions
        [Only sessionCount] <- doQuery_ connPool [sql| SELECT count(*) FROM thentos_sessions |]
        sessionCount `shouldBe` (1 :: Int)

  where
    token = "thentos session token"

garbageCollectServiceSessionsSpec :: SpecWith (Pool Connection)
garbageCollectServiceSessionsSpec = describe "garbageCollectServiceSessions" $ do
    it "deletes (only) expired service sessions" $ \connPool -> do
        Right _ <- runQuery connPool $ addUserPrim (Just testUid) testUser True
        hashedKey <- hashServiceKey "secret"
        Right _ <- runQuery connPool $
            addService (UserA testUid) sid hashedKey "sName" "sDescription"

        Right _ <- runQuery connPool $ startThentosSession tTok1 (UserA testUid) laterTimeout
        Right _ <- runQuery connPool $ startThentosSession tTok2 (UserA testUid) laterTimeout
        Right () <- runQuery connPool $ startServiceSession tTok1 sTok1 sid laterTimeout
        Right () <- runQuery connPool $ startServiceSession tTok2 sTok2 sid immediateTimeout
        Right () <- runQuery connPool garbageCollectServiceSessions

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


-- * Utils

mkUser :: UserName -> SBS -> ST -> User
mkUser name pass email = User { _userName = name
                              , _userPassword = encryptTestSecret pass
                              , _userEmail = forceUserEmail email
                              }

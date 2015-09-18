{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Thentos.TransactionSpec (spec) where

import Control.Applicative ((<$>))
import qualified Data.Set as Set
import Control.Lens ((&), (^.), (.~))
import Control.Monad (void)
import Data.Either (isRight)
import Data.List (sort)
import Data.String.Conversions (ST, SBS)
import Data.Thyme (fromSeconds')
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Test.Hspec (Spec, SpecWith, describe, it, shouldBe, shouldReturn, shouldSatisfy, before)

import Thentos.Action.Core
import Thentos.Transaction
import Thentos.Types
import Thentos.Util (hashUserPass, hashServiceKey)

import Thentos.Test.Core
import Thentos.Test.Config
import Thentos.Test.Transaction

spec :: Spec
spec = describe "Thentos.Transaction" . before (createActionState "test_thentos" thentosTestConfig)
  $ do
    addUserPrimSpec
    addUserSpec
    addUnconfirmedUserSpec
    addUnconfirmedUserWithIdSpec
    finishUserRegistrationSpec
    finishUserRegistrationByIdSpec
    lookupUserByNameSpec
    lookupUserByEmailSpec
    deleteUserSpec
    passwordResetTokenSpec
    updateUserFieldSpec
    agentRolesSpec
    assignRoleSpec
    unassignRoleSpec
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


addUserPrimSpec :: SpecWith ActionState
addUserPrimSpec = describe "addUserPrim" $ do

    it "adds a user to the database" $ \ (ActionState (connPool, _, _)) -> do
        let user   = testUsers !! 2
            userId = UserId 289
        void $ runQuery connPool $ addUserPrim (Just userId) user True
        Right (_, res) <- runQuery connPool $ lookupUser userId
        res `shouldBe` user

    it "fails if the id is not unique" $ \ (ActionState (connPool, _, _)) -> do
        let userId = UserId 289
        void $ runQuery connPool $ addUserPrim (Just userId) (testUsers !! 2) True
        x <- runQuery connPool $ addUserPrim (Just userId) (testUsers !! 3) True
        x `shouldBe` Left UserIdAlreadyExists

    it "fails if the username is not unique" $ \ (ActionState (connPool, _, _)) -> do
        let user1 = mkUser "name" "pass1" "email1@email.com"
            user2 = mkUser "name" "pass2" "email2@email.com"
        void $ runQuery connPool $ addUserPrim (Just $ UserId 372) user1 True
        x <- runQuery connPool $ addUserPrim (Just $ UserId 482) user2 True
        x `shouldBe` Left UserNameAlreadyExists

    it "fails if the email is not unique" $  \ (ActionState (connPool, _, _)) -> do
        let user1 = mkUser "name1" "pass1" "email@email.com"
            user2 = mkUser "name2" "pass2" "email@email.com"
        void $ runQuery connPool $ addUserPrim (Just $ UserId 372) user1 True
        x <- runQuery connPool $ addUserPrim (Just $ UserId 482) user2 True
        x `shouldBe` Left UserEmailAlreadyExists

addUserSpec :: SpecWith ActionState
addUserSpec = describe "addUser" $ do

    it "adds a user to the database" $ \ (ActionState (connPool, _, _)) -> do
        void $ runQuery connPool $ mapM_ addUser testUsers
        let names = _userName <$> testUsers
        Right res <- runQuery connPool $ mapM lookupUserByName names
        (snd <$> res) `shouldBe` testUsers

addUnconfirmedUserSpec :: SpecWith ActionState
addUnconfirmedUserSpec = describe "addUnconfirmedUser" $ do
    let user  = mkUser "name" "pass" "email@email.com"
        token = "sometoken"

    it "adds a user to the database" $ \ (ActionState (connPool, _, _)) -> do
        Right uid <- runQuery connPool $ addUnconfirmedUser token user
        Right (_, usr) <- runQuery connPool $ lookupUser uid
        usr `shouldBe` user

addUnconfirmedUserWithIdSpec :: SpecWith ActionState
addUnconfirmedUserWithIdSpec = describe "addUnconfirmedUserWithId" $ do
    let user   = mkUser "name" "pass" "email@email.com"
        userid = UserId 321
        token  = "sometoken"

    it "adds an unconfirmed user to the DB" $ \ (ActionState (connPool, _, _)) -> do
        Right () <- runQuery connPool $ addUnconfirmedUserWithId token user userid
        Right (_, usr) <- runQuery connPool $ lookupUserByName "name"
        usr `shouldBe` user

    it "adds the token for the user to the DB" $ \ (ActionState (connPool, _, _)) -> do
        Right () <- runQuery connPool $ addUnconfirmedUserWithId token user userid
        [Only res] <- doQuery connPool [sql|
            SELECT token FROM user_confirmation_tokens
            WHERE id = ? |] (Only userid)
        res `shouldBe` token

    it "fails if the token is not unique" $ \ (ActionState (connPool, _, _)) -> do
        let user2 = mkUser "name2" "pass" "email2@email.com"
            userid2 = UserId 322
        Right () <- runQuery connPool $ addUnconfirmedUserWithId token user userid
        Left err <- runQuery connPool $ addUnconfirmedUserWithId token user2 userid2
        err `shouldBe` ConfirmationTokenAlreadyExists

finishUserRegistrationByIdSpec :: SpecWith ActionState
finishUserRegistrationByIdSpec = describe "finishUserRegistrationById" $ do
    let user   = mkUser "name" "pass" "email@email.com"
        userid = UserId 321
        token  = "sometoken"

    it "makes the user be confirmed" $ \ (ActionState (connPool, _, _)) -> do
        Right () <- runQuery connPool $ addUnconfirmedUserWithId token user userid
        [Only res1] <- doQuery connPool [sql|
            SELECT confirmed FROM "users"
            WHERE id = ? |] (Only userid)
        res1 `shouldBe` False
        Right () <- runQuery connPool $ finishUserRegistrationById userid
        [Only res2] <- doQuery connPool [sql|
            SELECT confirmed FROM "users"
            WHERE id = ? |] (Only userid)
        res2 `shouldBe` True

    it "removes the confirmation token" $ \ (ActionState (connPool, _, _)) -> do
        Right () <- runQuery connPool $ addUnconfirmedUserWithId token user userid
        Right () <- runQuery connPool $ finishUserRegistrationById userid
        res <- doQuery connPool [sql|
            SELECT token FROM user_confirmation_tokens
            WHERE id = ? |] (Only userid)
        res `shouldBe` ([] :: [Only ConfirmationToken])

    it "fails if the user is already confirmed" $ \ (ActionState (connPool, _, _)) -> do
        Right () <- runQuery connPool $ addUnconfirmedUserWithId token user userid
        Right () <- runQuery connPool $ finishUserRegistrationById userid
        Left err <- runQuery connPool $ finishUserRegistrationById userid
        err `shouldBe` NoSuchPendingUserConfirmation

    it "fails if the user doesn't exist" $ \ (ActionState (connPool, _, _)) -> do
        Left err <- runQuery connPool $ finishUserRegistrationById userid
        err `shouldBe` NoSuchPendingUserConfirmation

finishUserRegistrationSpec :: SpecWith ActionState
finishUserRegistrationSpec = describe "finishUserRegistration" $ do
    it "confirms the user if the given token exists" $ \(ActionState (connPool, _, _)) -> do
        Right uid <- runQuery connPool $ addUnconfirmedUser token testUser
        Right uid' <- runQuery connPool $ finishUserRegistration timeout token
        uid `shouldBe` uid'
        [Only confirmed] <- doQuery connPool
            [sql| SELECT confirmed FROM users WHERE id = ? |] (Only uid)
        confirmed `shouldBe` True
        [Only tokenCount] <- doQuery_ connPool
            [sql| SELECT COUNT(*) FROM user_confirmation_tokens |]
        tokenCount `shouldBe` (0 :: Int)

    it "fails if the given token does not exist" $ \(ActionState (connPool, _, _)) -> do
        Right uid <- runQuery connPool $ addUnconfirmedUser token testUser
        Left NoSuchToken <- runQuery connPool $ finishUserRegistration timeout "badToken"
        [Only confirmed] <- doQuery connPool
            [sql| SELECT confirmed FROM users WHERE id = ? |] (Only uid)
        confirmed `shouldBe` False

  where
    token = "someToken"
    timeout = Timeout $ fromSeconds' 60

lookupUserByNameSpec :: SpecWith ActionState
lookupUserByNameSpec = describe "lookupUserByName" $ do

    it "returns a user if one exists" $ \ (ActionState (connPool, _, _)) -> do
        let user = mkUser "name" "pass" "email@email.com"
            userid = UserId 437
        void $ runQuery connPool $ addUserPrim (Just userid) user True
        runQuery connPool (lookupUserByName "name") `shouldReturn` Right (userid, user)

    it "returns NoSuchUser if no user has the name" $ \ (ActionState (connPool, _, _)) -> do
        runQuery connPool (lookupUserByName "name") `shouldReturn` Left NoSuchUser

lookupUserByEmailSpec :: SpecWith ActionState
lookupUserByEmailSpec = describe "lookupUserByEmail" $ do

    it "returns a user if one exists" $ \ (ActionState (connPool, _, _)) -> do
        let user = mkUser "name" "pass" "email@email.com"
            userid = UserId 437
        void $ runQuery connPool $ addUserPrim (Just userid) user True
        runQuery connPool (lookupUserByEmail $ forceUserEmail "email@email.com")
            `shouldReturn` Right (userid, user)

    it "returns NoSuchUser if no user has the email" $ \ (ActionState (connPool, _, _)) -> do
        runQuery connPool (lookupUserByName "name") `shouldReturn` Left NoSuchUser

deleteUserSpec :: SpecWith ActionState
deleteUserSpec = describe "deleteUser" $ do

    it "deletes a user" $ \ (ActionState (connPool, _, _)) -> do
        let user = mkUser "name" "pass" "email@email.com"
            userid = UserId 371
        void $ runQuery connPool $ addUserPrim (Just userid) user True
        Right _  <- runQuery connPool $ lookupUser userid
        Right () <- runQuery connPool $ deleteUser userid
        runQuery connPool (lookupUser userid) `shouldReturn` Left NoSuchUser

    it "throws NoSuchUser if the id does not exist" $ \ (ActionState (connPool, _, _)) -> do
        runQuery connPool (deleteUser $ UserId 210) `shouldReturn` Left NoSuchUser

passwordResetTokenSpec :: SpecWith ActionState
passwordResetTokenSpec = describe "addPasswordResetToken" $ do
    it "adds a password reset to the db" $ \(ActionState (connPool, _, _)) -> do
        let user = mkUser "name" "super secret" "me@example.com"
            userId = UserId 584
            testToken = PasswordResetToken "asgbagbaosubgoas"
        Right _ <- runQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runQuery connPool $
            addPasswordResetToken (user ^. userEmail) testToken
        [Only token_in_db] <- doQuery connPool
            [sql| SELECT token FROM password_reset_tokens |] ()
        token_in_db `shouldBe` testToken

    it "resets a password if the given token exists" $ \(ActionState (connPool, _, _)) -> do
        let user = mkUser "name" "super secret" "me@example.com"
            userId = UserId 594
            testToken = PasswordResetToken "asgbagbaosubgoas"
        newEncryptedPass <- hashUserPass "newSecretP4ssw0rd"
        Right _ <- runQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runQuery connPool $
            addPasswordResetToken (user ^. userEmail) testToken
        Right _ <- runQuery connPool $
            resetPassword (Timeout 3600) testToken newEncryptedPass
        [Only newPassInDB] <- doQuery connPool
            [sql| SELECT password FROM users WHERE id = ?|] (Only userId)
        newPassInDB `shouldBe` newEncryptedPass

updateUserFieldSpec :: SpecWith ActionState
updateUserFieldSpec = describe "updateUserField" $ do
    let user = mkUser "name" "super secret" "me@example.com"
        userId = UserId 111
        user2 = mkUser "someone" "ppppp" "who@example.com"
        user2Id = UserId 222

    it "changes the user name" $ \(ActionState (connPool, _, _)) -> do
        let newName = UserName "new"
        Right _ <- runQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runQuery connPool $
            updateUserField userId (UpdateUserFieldName newName)
        Right (_, usr) <- runQuery connPool $ lookupUser userId
        usr `shouldBe` (user & userName .~ newName)

    it "changes the user email" $ \(ActionState (connPool, _, _)) -> do
        let newEmail = forceUserEmail "new@example.com"
        Right _ <- runQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runQuery connPool $
            updateUserField userId (UpdateUserFieldEmail newEmail)
        Right (_, usr) <- runQuery connPool $ lookupUser userId
        usr `shouldBe` (user & userEmail .~ newEmail)

    it "changes the user name" $ \(ActionState (connPool, _, _)) -> do
        let newPass = encryptTestSecret "new pass"
        Right _ <- runQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runQuery connPool $
            updateUserField userId (UpdateUserFieldPassword newPass)
        Right (_, usr) <- runQuery connPool $ lookupUser userId
        usr `shouldBe` (user & userPassword .~ newPass)

    it "doesn't change other users" $ \(ActionState (connPool, _, _)) -> do
        let newName = UserName "new"
        Right _ <- runQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runQuery connPool $ addUserPrim (Just user2Id) user2 True
        Right _ <- runQuery connPool $
            updateUserField userId (UpdateUserFieldName newName)
        Right (_, usr2) <- runQuery connPool $ lookupUser user2Id
        usr2 `shouldBe` user2

    it "fails if the new name is not unique" $ \(ActionState (connPool, _, _)) -> do
        let newName = user2 ^. userName
        Right _ <- runQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runQuery connPool $ addUserPrim (Just user2Id) user2 True
        x <- runQuery connPool $ updateUserField userId (UpdateUserFieldName newName)
        x `shouldBe` Left UserNameAlreadyExists
        runQuery connPool (lookupUser userId) `shouldReturn` Right (userId, user)
        runQuery connPool (lookupUser user2Id) `shouldReturn` Right (user2Id, user2)

    it "fails if the user doesn't exist" $ \(ActionState (connPool, _, _)) -> do
        x <- runQuery connPool $ updateUserField userId $ UpdateUserFieldName $ UserName "nobody"
        x `shouldBe` Left NoSuchUser

agentRolesSpec :: SpecWith ActionState
agentRolesSpec = describe "agentRoles" $ do
    it "returns an empty set for a user or service without roles" $
      \(ActionState (connPool, _, _)) -> do
        Right _ <- runQuery connPool $ addUserPrim (Just testUid) testUser True
        x <- runQuery connPool $ agentRoles (UserA testUid)
        x `shouldBe` Right []

        Right _ <- runQuery connPool $
            addService (UserA testUid) sid testHashedSecret "name" "desc"
        roles <- runQuery connPool $ agentRoles (ServiceA sid)
        roles `shouldBe` Right []
  where
    sid = "sid"

assignRoleSpec :: SpecWith ActionState
assignRoleSpec = describe "assignRole" $ do
    it "adds a role" $ \(ActionState (connPool, _, _)) -> do
        Right _ <- runQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runQuery connPool $ assignRole (UserA testUid) RoleAdmin
        Right roles <- runQuery connPool $ agentRoles (UserA testUid)
        roles `shouldBe` [RoleAdmin]

        addTestService connPool
        Right _ <- runQuery connPool $ assignRole (ServiceA sid) RoleAdmin
        Right serviceRoles <- runQuery connPool $ agentRoles (ServiceA sid)
        serviceRoles `shouldBe` [RoleAdmin]

    it "silently allows adding a duplicate role" $ \(ActionState (connPool, _, _)) -> do
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

    it "adds a second role" $ \(ActionState (connPool, _, _)) -> do
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

unassignRoleSpec :: SpecWith ActionState
unassignRoleSpec = describe "unassignRole" $ do
    it "silently allows removing a non-assigned role" $ \(ActionState (connPool, _, _)) -> do
        Right _ <- runQuery connPool $ addUserPrim (Just testUid) testUser True
        x <- runQuery connPool $ unassignRole (UserA testUid) RoleAdmin
        x `shouldBe` Right ()

        addTestService connPool
        res <- runQuery connPool $ unassignRole (ServiceA sid) RoleAdmin
        res `shouldBe` Right ()

    it "removes the specified role" $ \(ActionState (connPool, _, _)) -> do
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

emailChangeRequestSpec :: SpecWith ActionState
emailChangeRequestSpec = describe "addUserEmailChangeToken" $ do
    it "adds an email change token to the db" $ \(ActionState (connPool, _, _)) -> do
        Right _ <- runThentosQueryFromPool connPool $ addUserPrim (Just userId) user True
        Right _ <- runThentosQueryFromPool connPool $
            addUserEmailChangeRequest userId newEmail testToken
        [Only tokenInDb] <- doQuery connPool
            [sql| SELECT token FROM email_change_tokens|] ()
        tokenInDb `shouldBe` testToken

    it "changes a user's email if given a valid token" $ \(ActionState (connPool, _, _)) -> do
        Right _ <- runThentosQueryFromPool connPool $ addUserPrim (Just userId) user True
        Right _ <- runThentosQueryFromPool connPool $
            addUserEmailChangeRequest userId newEmail testToken
        Right _ <-
            runThentosQueryFromPool connPool $ confirmUserEmailChange (Timeout 3600) testToken
        [Only expectedEmail] <- doQuery connPool
            [sql| SELECT email FROM users WHERE id = ?|] (Only userId)
        expectedEmail `shouldBe` newEmail
  where
    user = mkUser "name" "super secret" "me@example.com"
    userId = UserId 584
    testToken = ConfirmationToken "asgbagbaosubgoas"
    newEmail = forceUserEmail "new@example.com"

addServiceSpec :: SpecWith ActionState
addServiceSpec = describe "addService" $ do
    it "adds a service to the db" $ \(ActionState (connPool, _, _)) -> do
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

    it "allows a service's owner to be a service" $ \(ActionState (connPool, _, _)) -> do
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

deleteServiceSpec :: SpecWith ActionState
deleteServiceSpec = describe "deleteService" $ do
    it "deletes a service" $ \(ActionState (connPool, _, _)) -> do
        Right _ <- runThentosQueryFromPool connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runThentosQueryFromPool connPool $
            addService (UserA testUid) sid testHashedSecret "" ""
        Right _ <- runThentosQueryFromPool connPool $ deleteService sid
        [Only serviceCount] <- doQuery connPool [sql| SELECT COUNT(*) FROM services |] ()
        serviceCount `shouldBe` (0 :: Int)

    it "fails if the service doesn't exist" $ \(ActionState (connPool, _, _)) -> do
        Left NoSuchService <- runThentosQueryFromPool connPool $ deleteService sid
        return ()
  where
    sid = ServiceId "blablabla"

lookupServiceSpec :: SpecWith ActionState
lookupServiceSpec = describe "lookupService" $ do
    it "looks up a service"  $ \(ActionState (connPool, _, _)) -> do
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

startThentosSessionSpec :: SpecWith ActionState
startThentosSessionSpec = describe "startThentosSession" $ do
    let tok = "something"
        user = UserA (UserId 55)
        period = Timeout $ fromSeconds' 60

    it "creates a thentos session for a user" $ \(ActionState (connPool, _, _)) -> do
        void $ runQuery connPool $ addUserPrim (Just testUid) testUser True
        Right () <- runQuery connPool $ startThentosSession tok (UserA testUid) period
        [Only uid] <- doQuery connPool [sql| SELECT uid
                                             FROM thentos_sessions
                                             WHERE token = ? |] (Only tok)
        uid `shouldBe` testUid

    it "fails when the user doesn't exist" $ \(ActionState (connPool, _, _)) -> do
        x <- runQuery connPool $ startThentosSession tok user period
        x `shouldBe` Left NoSuchUser

    it "creates a thentos session for a service" $ \(ActionState (connPool, _, _)) -> do
        let sid = "sid"
        void $ runQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runThentosQueryFromPool connPool $
            addService (UserA testUid) sid testHashedSecret "name" "desc"
        Right () <- runQuery connPool $ startThentosSession tok (ServiceA sid) period
        [Only sid'] <- doQuery connPool [sql| SELECT sid
                                         FROM thentos_sessions
                                         WHERE token = ? |] (Only tok)
        sid' `shouldBe` sid

    it "fails when the service doesn't exist" $ \(ActionState (connPool, _, _)) -> do
        x <- runQuery connPool $ startThentosSession tok (ServiceA "sid") period
        x `shouldBe` Left NoSuchService

lookupThentosSessionSpec :: SpecWith ActionState
lookupThentosSessionSpec = describe "lookupThentosSession" $ do
    let tok = ThentosSessionToken "hellohello"
        userId = UserId 777
        user = mkUser "name" "pass" "email@email.com"
        agent = UserA userId
        period' = fromSeconds' 60
        period = Timeout period'
        sid = "sid"

    it "fails when there is no session" $ \(ActionState (connPool, _, _)) -> do
        void $ runQuery connPool $ addUserPrim (Just userId) user True
        x <- runQuery connPool $ lookupThentosSession tok
        x `shouldBe` Left NoSuchThentosSession

    it "reads back a fresh session" $ \(ActionState (connPool, _, _)) -> do
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

    it "fails for an expired session" $ \(ActionState (connPool, _, _)) -> do
        void $ runQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runQuery connPool $ startThentosSession tok agent (Timeout $ fromSeconds' 0)
        x <- runQuery connPool $ lookupThentosSession tok
        x `shouldBe` Left NoSuchThentosSession

    it "extends the session" $ \(ActionState (connPool, _, _)) -> do
        void $ runQuery connPool $ addUserPrim (Just userId) user True
        Right _ <- runQuery connPool $ startThentosSession tok agent period
        Right (_, sess1) <- runQuery connPool $ lookupThentosSession tok
        Right (t, sess2) <- runQuery connPool $ lookupThentosSession tok
        t `shouldBe` tok
        sess1 ^. thSessEnd `shouldSatisfy` (< sess2 ^. thSessEnd)

endThentosSessionSpec :: SpecWith ActionState
endThentosSessionSpec = describe "endThentosSession" $ do
    let tok = "something"
        userId = UserId 777
        user = mkUser "name" "pass" "email@email.com"
        agent = UserA userId
        period = Timeout $ fromSeconds' 60

    it "deletes a session" $ \(ActionState (connPool, _, _)) -> do
        void $ runQuery connPool $ addUserPrim (Just userId) user True
        void $ runQuery connPool $ startThentosSession tok agent period
        Right _ <- runQuery connPool $ lookupThentosSession tok
        Right _ <- runQuery connPool $ endThentosSession tok
        x <- runQuery connPool $ lookupThentosSession tok
        x `shouldBe` Left NoSuchThentosSession

    it "silently allows deleting a non-existing session" $ \(ActionState (connPool, _, _)) -> do
        x <- runQuery connPool $ endThentosSession tok
        x `shouldSatisfy` isRight

serviceNamesFromThentosSessionSpec :: SpecWith ActionState
serviceNamesFromThentosSessionSpec = describe "serviceNamesFromThentosSession" $ do
    it "gets the names of all services that a thentos session is signed into" $ \(ActionState (conn, _, _)) -> do
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
    period = Timeout $ fromSeconds' 60

startServiceSessionSpec :: SpecWith ActionState
startServiceSessionSpec = describe "startServiceSession" $ do
    it "starts a service session" $ \(ActionState (connPool, _, _)) -> do
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
    period = Timeout $ fromSeconds' 60
    thentosSessionToken = "foo"
    serviceSessionToken = "bar"
    sid = "sid"

endServiceSessionSpec :: SpecWith ActionState
endServiceSessionSpec = describe "endServiceSession" $ do
    it "ends an service session" $ \(ActionState  (connPool, _, _)) -> do
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
    period = Timeout $ fromSeconds' 60
    thentosSessionToken = "foo"
    serviceSessionToken = "bar"
    sid = "sid"

lookupServiceSessionSpec :: SpecWith ActionState
lookupServiceSessionSpec = describe "lookupServiceSession" $ do
    it "looks up the service session with a given token" $ \(ActionState (connPool, _, _)) -> do
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
      \(ActionState (connPool, _, _)) -> do
        Left err <- runQuery connPool $ lookupServiceSession "non-existent token"
        err `shouldBe` NoSuchServiceSession

  where
    period = Timeout $ fromSeconds' 60
    thentosSessionToken = "foo"
    serviceSessionToken = "bar"
    sid = "sid"


-- * Garbage collection

garbageCollectUnconfirmedUsersSpec :: SpecWith ActionState
garbageCollectUnconfirmedUsersSpec = describe "garbageCollectUnconfirmedUsers" $ do
    let user1   = mkUser "name1" "pass" "email1@email.com"
        userid1 = UserId 321
        token1  = "sometoken1"
        user2   = mkUser "name2" "pass" "email2@email.com"
        userid2 = UserId 322
        token2  = "sometoken2"

    it "deletes all expired unconfirmed users" $ \ (ActionState (connPool, _, _)) -> do
        Right () <- runQuery connPool $ addUnconfirmedUserWithId token1 user1 userid1
        Right () <- runQuery connPool $ addUnconfirmedUserWithId token2 user2 userid2
        Right () <- runQuery connPool $ garbageCollectUnconfirmedUsers 0
        [Only tkns] <- doQuery_ connPool [sql| SELECT count(*) FROM user_confirmation_tokens |]
        [Only usrs] <- doQuery_ connPool [sql| SELECT count(*) FROM "users" |]
        tkns `shouldBe` (0 :: Int)
        usrs `shouldBe` (0 :: Int)

    it "only deletes expired unconfirmed users" $ \ (ActionState (connPool, _, _)) -> do
        Right () <- runQuery connPool $ addUnconfirmedUserWithId token1 user1 userid1
        Right () <- runQuery connPool $ garbageCollectUnconfirmedUsers 100000
        [Only tkns] <- doQuery_ connPool [sql| SELECT count(*) FROM user_confirmation_tokens |]
        [Only usrs] <- doQuery_ connPool [sql| SELECT count(*) FROM "users" |]
        tkns `shouldBe` (1 :: Int)
        usrs `shouldBe` (1 :: Int)

garbageCollectPasswordResetTokensSpec :: SpecWith ActionState
garbageCollectPasswordResetTokensSpec = describe "garbageCollectPasswordResetTokens" $ do
    let user   = mkUser "name1" "pass" "email1@email.com"
        userid = UserId 321
        email = forceUserEmail "email1@email.com"
        passToken = "sometoken2"

    it "deletes all expired tokens" $ \ (ActionState (connPool, _, _)) -> do
        void $ runQuery connPool $ addUserPrim (Just userid) user True
        void $ runQuery connPool $ addPasswordResetToken email passToken
        [Only tkns] <- doQuery_ connPool [sql| SELECT count(*) FROM password_reset_tokens |]
        tkns `shouldBe` (1 :: Int)
        Right () <- runQuery connPool $ garbageCollectPasswordResetTokens 0
        [Only tkns'] <- doQuery_ connPool [sql| SELECT count(*) FROM password_reset_tokens |]
        tkns' `shouldBe` (0 :: Int)

    it "only deletes expired tokens" $ \ (ActionState (connPool, _, _)) -> do
        void $ runQuery connPool $ addUserPrim (Just userid) user True
        void $ runQuery connPool $ addPasswordResetToken email passToken
        void $ runQuery connPool $ garbageCollectPasswordResetTokens 1000000
        [Only tkns'] <- doQuery_ connPool [sql| SELECT count(*) FROM password_reset_tokens |]
        tkns' `shouldBe` (1 :: Int)

garbageCollectEmailChangeTokensSpec :: SpecWith ActionState
garbageCollectEmailChangeTokensSpec = describe "garbageCollectEmailChangeTokens" $ do
    let newEmail = forceUserEmail "new@example.com"
        token = "sometoken2"

    it "deletes all expired tokens" $ \(ActionState (connPool, _, _)) -> do
        Right _ <- runQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runQuery connPool $ addUserEmailChangeRequest testUid newEmail token
        [Only tokenCount] <- doQuery_ connPool [sql| SELECT count(*) FROM email_change_tokens |]
        tokenCount `shouldBe` (1 :: Int)
        Right () <- runQuery connPool $ garbageCollectEmailChangeTokens 0
        [Only tokenCount'] <- doQuery_ connPool [sql| SELECT count(*) FROM email_change_tokens |]
        tokenCount' `shouldBe` (0 :: Int)

    it "only deletes expired tokens" $ \ (ActionState (connPool, _, _)) -> do
        Right _ <- runQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runQuery connPool $ addUserEmailChangeRequest testUid newEmail token
        Right () <- runQuery connPool $ garbageCollectEmailChangeTokens 1000000
        [Only tkns'] <- doQuery_ connPool [sql| SELECT count(*) FROM email_change_tokens |]
        tkns' `shouldBe` (1 :: Int)

garbageCollectThentosSessionsSpec :: SpecWith ActionState
garbageCollectThentosSessionsSpec = describe "garbageCollectThentosSessions" $ do
    it "deletes all expired thentos sessions" $ \(ActionState (connPool, _, _)) -> do
        let immediateTimeout = Timeout $ fromSeconds' 0
        Right _ <- runQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runQuery connPool $ startThentosSession token (UserA testUid) immediateTimeout
        Right () <- runQuery connPool garbageCollectThentosSessions
        [Only sessionCount] <- doQuery_ connPool [sql| SELECT count(*) FROM thentos_sessions |]
        sessionCount `shouldBe` (0 :: Int)

    it "doesn't delete active sessions" $ \(ActionState (connPool, _, _)) -> do
        let timeout = Timeout $ fromSeconds' 60
        Right _ <- runQuery connPool $ addUserPrim (Just testUid) testUser True
        Right _ <- runQuery connPool $ startThentosSession token (UserA testUid) timeout
        Right () <- runQuery connPool garbageCollectThentosSessions
        [Only sessionCount] <- doQuery_ connPool [sql| SELECT count(*) FROM thentos_sessions |]
        sessionCount `shouldBe` (1 :: Int)

  where
    token = "thentos session token"

garbageCollectServiceSessionsSpec :: SpecWith ActionState
garbageCollectServiceSessionsSpec = describe "garbageCollectServiceSessions" $ do
    it "deletes (only) expired service sessions" $ \(ActionState (connPool, _, _)) -> do
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
    immediateTimeout = Timeout $ fromSeconds' 0
    laterTimeout = Timeout $ fromSeconds' 60


-- * Utils

mkUser :: UserName -> SBS -> ST -> User
mkUser name pass email = User { _userName = name
                              , _userPassword = encryptTestSecret pass
                              , _userEmail = forceUserEmail email
                              }

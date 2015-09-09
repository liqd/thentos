{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Thentos.TransactionSpec (spec) where

import Control.Applicative ((<$>))
import Data.Monoid (mempty)
import qualified Data.Set as Set
import Control.Lens ((&), (^.), (.~))
import Control.Monad (void)
import Data.Either (isRight)
import Data.String.Conversions (ST, SBS)
import Data.Thyme (fromSeconds')
import Data.Void (Void)
import Database.PostgreSQL.Simple (Only(..), Connection, query, query_)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Test.Hspec (Spec, SpecWith, describe, it, shouldBe, shouldReturn, shouldSatisfy, before)

import Thentos.Action.Core
import Thentos.Transaction
import Thentos.Transaction.Core
import Thentos.Types
import Thentos.Util (hashUserPass, hashServiceKey)

import Thentos.Test.Core
import Thentos.Test.Config

spec :: Spec
spec = describe "Thentos.Transaction" . before (createActionState "test_thentos" thentosTestConfig) $ do
    addUserPrimSpec
    addUserSpec
    addUnconfirmedUserSpec
    addUnconfirmedUserWithIdSpec
    finishUserRegistrationByIdSpec
    lookupUserByNameSpec
    lookupUserByEmailSpec
    deleteUserSpec
    passwordResetTokenSpec
    updateUserFieldSpec
    agentRolesSpec
    assignRoleSpec
    unassignRoleSpec
    doGarbageCollectUnconfirmedUsersSpec
    garbageCollectPasswordResetTokensSpec
    garbageCollectEmailChangeTokensSpec
    emailChangeRequestSpec
    addServiceSpec
    deleteServiceSpec
    lookupThentosSessionSpec
    startThentosSessionSpec
    endThentosSessionSpec
    startServiceSessionSpec
    endServiceSessionSpec


addUserPrimSpec :: SpecWith ActionState
addUserPrimSpec = describe "addUserPrim" $ do

    it "adds a user to the database" $ \ (ActionState (conn, _, _)) -> do
        let user   = testUsers !! 2
            userId = UserId 289
        void $ runQuery conn $ addUserPrim userId user
        Right (_, res) <- runQuery conn $ lookupUser userId
        res `shouldBe` user

    it "fails if the id is not unique" $ \ (ActionState (conn, _, _)) -> do
        let userId = UserId 289
        void $ runQuery conn $ addUserPrim userId (testUsers !! 2)
        x <- runQuery conn $ addUserPrim userId (testUsers !! 3)
        x `shouldBe` Left UserIdAlreadyExists

    it "fails if the username is not unique" $ \ (ActionState (conn, _, _)) -> do
        let user1 = mkUser "name" "pass1" "email1@email.com"
            user2 = mkUser "name" "pass2" "email2@email.com"
        void $ runQuery conn $ addUserPrim (UserId 372) user1
        x <- runQuery conn $ addUserPrim (UserId 482) user2
        x `shouldBe` Left UserNameAlreadyExists

    it "fails if the email is not unique" $  \ (ActionState (conn, _, _)) -> do
        let user1 = mkUser "name1" "pass1" "email@email.com"
            user2 = mkUser "name2" "pass2" "email@email.com"
        void $ runQuery conn $ addUserPrim (UserId 372) user1
        x <- runQuery conn $ addUserPrim (UserId 482) user2
        x `shouldBe` Left UserEmailAlreadyExists

addUserSpec :: SpecWith ActionState
addUserSpec = describe "addUser" $ do

    it "adds a user to the database" $ \ (ActionState (conn, _, _)) -> do
        void $ runQuery conn $ mapM_ addUser testUsers
        let names = _userName <$> testUsers
        Right res <- runQuery conn $ mapM lookupUserByName names
        (snd <$> res) `shouldBe` testUsers

addUnconfirmedUserSpec :: SpecWith ActionState
addUnconfirmedUserSpec = describe "addUnconfirmedUser" $ do
    let user  = mkUser "name" "pass" "email@email.com"
        token = "sometoken"

    it "adds a user to the database" $ \ (ActionState (conn, _, _)) -> do
        Right uid <- runQuery conn $ addUnconfirmedUser token user
        Right (_, usr) <- runQuery conn $ lookupUser uid
        usr `shouldBe` user

addUnconfirmedUserWithIdSpec :: SpecWith ActionState
addUnconfirmedUserWithIdSpec = describe "addUnconfirmedUserWithId" $ do
    let user   = mkUser "name" "pass" "email@email.com"
        userid = UserId 321
        token  = "sometoken"

    it "adds an unconfirmed user to the DB" $ \ (ActionState (conn, _, _)) -> do
        Right () <- runQuery conn $ addUnconfirmedUserWithId token user userid
        Right (_, usr) <- runQuery conn $ lookupUserByName "name"
        usr `shouldBe` user

    it "adds the token for the user to the DB" $ \ (ActionState (conn, _, _)) -> do
        Right () <- runQuery conn $ addUnconfirmedUserWithId token user userid
        [Only res] <- query conn [sql|
            SELECT token FROM user_confirmation_tokens
            WHERE id = ? |] (Only userid)
        res `shouldBe` token

    it "fails if the token is not unique" $ \ (ActionState (conn, _, _)) -> do
        let user2 = mkUser "name2" "pass" "email2@email.com"
            userid2 = UserId 322
        Right () <- runQuery conn $ addUnconfirmedUserWithId token user userid
        Left err <- runQuery conn $ addUnconfirmedUserWithId token user2 userid2
        err `shouldBe` ConfirmationTokenAlreadyExists

finishUserRegistrationByIdSpec :: SpecWith ActionState
finishUserRegistrationByIdSpec = describe "finishUserRegistrationById" $ do
    let user   = mkUser "name" "pass" "email@email.com"
        userid = UserId 321
        token  = "sometoken"

    it "makes the user be confirmed" $ \ (ActionState (conn, _, _)) -> do
        Right () <- runQuery conn $ addUnconfirmedUserWithId token user userid
        [Only res1] <- query conn [sql|
            SELECT confirmed FROM "users"
            WHERE id = ? |] (Only userid)
        res1 `shouldBe` False
        Right () <- runQuery conn $ finishUserRegistrationById userid
        [Only res2] <- query conn [sql|
            SELECT confirmed FROM "users"
            WHERE id = ? |] (Only userid)
        res2 `shouldBe` True

    it "removes the confirmation token" $ \ (ActionState (conn, _, _)) -> do
        Right () <- runQuery conn $ addUnconfirmedUserWithId token user userid
        Right () <- runQuery conn $ finishUserRegistrationById userid
        res <- query conn [sql|
            SELECT token FROM user_confirmation_tokens
            WHERE id = ? |] (Only userid)
        res `shouldBe` ([] :: [Only ConfirmationToken])

    it "fails if the user is already confirmed" $ \ (ActionState (conn, _, _)) -> do
        Right () <- runQuery conn $ addUnconfirmedUserWithId token user userid
        Right () <- runQuery conn $ finishUserRegistrationById userid
        Left err <- runQuery conn $ finishUserRegistrationById userid
        err `shouldBe` NoSuchPendingUserConfirmation

    it "fails if the user doesn't exist" $ \ (ActionState (conn, _, _)) -> do
        Left err <- runQuery conn $ finishUserRegistrationById userid
        err `shouldBe` NoSuchPendingUserConfirmation


lookupUserByNameSpec :: SpecWith ActionState
lookupUserByNameSpec = describe "lookupUserByName" $ do

    it "returns a user if one exists" $ \ (ActionState (conn, _, _)) -> do
        let user = mkUser "name" "pass" "email@email.com"
            userid = UserId 437
        void $ runQuery conn $ addUserPrim userid user
        runQuery conn (lookupUserByName "name") `shouldReturn` Right (userid, user)

    it "returns NoSuchUser if no user has the name" $ \ (ActionState (conn, _, _)) -> do
        runQuery conn (lookupUserByName "name") `shouldReturn` Left NoSuchUser

lookupUserByEmailSpec :: SpecWith ActionState
lookupUserByEmailSpec = describe "lookupUserByEmail" $ do

    it "returns a user if one exists" $ \ (ActionState (conn, _, _)) -> do
        let user = mkUser "name" "pass" "email@email.com"
            userid = UserId 437
        void $ runQuery conn $ addUserPrim userid user
        runQuery conn (lookupUserByEmail $ forceUserEmail "email@email.com")
            `shouldReturn` Right (userid, user)

    it "returns NoSuchUser if no user has the email" $ \ (ActionState (conn, _, _)) -> do
        runQuery conn (lookupUserByName "name") `shouldReturn` Left NoSuchUser

deleteUserSpec :: SpecWith ActionState
deleteUserSpec = describe "deleteUser" $ do

    it "deletes a user" $ \ (ActionState (conn, _, _)) -> do
        let user = mkUser "name" "pass" "email@email.com"
            userid = UserId 371
        void $ runQuery conn $ addUserPrim userid user
        Right _  <- runQuery conn $ lookupUser userid
        Right () <- runQuery conn $ deleteUser userid
        runQuery conn (lookupUser userid) `shouldReturn` Left NoSuchUser

    it "throws NoSuchUser if the id does not exist" $ \ (ActionState (conn, _, _)) -> do
        runQuery conn (deleteUser $ UserId 210) `shouldReturn` Left NoSuchUser

passwordResetTokenSpec :: SpecWith ActionState
passwordResetTokenSpec = describe "addPasswordResetToken" $ do
    it "adds a password reset to the db" $ \(ActionState (conn, _, _)) -> do
        let user = mkUser "name" "super secret" "me@example.com"
            userId = UserId 584
            testToken = PasswordResetToken "asgbagbaosubgoas"
        Right _ <- runQuery conn $ addUserPrim userId user
        Right _ <- runQuery conn $
            addPasswordResetToken (user ^. userEmail) testToken
        [Only token_in_db] <- query conn
            [sql| SELECT token FROM password_reset_tokens |] ()
        token_in_db `shouldBe` testToken

    it "resets a password if the given token exists" $ \(ActionState (conn, _, _)) -> do
        let user = mkUser "name" "super secret" "me@example.com"
            userId = UserId 594
            testToken = PasswordResetToken "asgbagbaosubgoas"
        newEncryptedPass <- hashUserPass "newSecretP4ssw0rd"
        Right _ <- runQuery conn $ addUserPrim userId user
        Right _ <- runQuery conn $
            addPasswordResetToken (user ^. userEmail) testToken
        Right _ <- runQuery conn $
            resetPassword (Timeout 3600) testToken newEncryptedPass
        [Only newPassInDB] <- query conn
            [sql| SELECT password FROM users WHERE id = ?|] (Only userId)
        newPassInDB `shouldBe` newEncryptedPass

updateUserFieldSpec :: SpecWith ActionState
updateUserFieldSpec = describe "updateUserField" $ do
    let user = mkUser "name" "super secret" "me@example.com"
        userId = UserId 111
        user2 = mkUser "someone" "ppppp" "who@example.com"
        user2Id = UserId 222

    it "changes the user name" $ \(ActionState (conn, _, _)) -> do
        let newName = UserName "new"
        Right _ <- runQuery conn $ addUserPrim userId user
        Right _ <- runQuery conn $
            updateUserField userId (UpdateUserFieldName newName)
        Right (_, usr) <- runQuery conn $ lookupUser userId
        usr `shouldBe` (user & userName .~ newName)

    it "changes the user email" $ \(ActionState (conn, _, _)) -> do
        let newEmail = forceUserEmail "new@example.com"
        Right _ <- runQuery conn $ addUserPrim userId user
        Right _ <- runQuery conn $
            updateUserField userId (UpdateUserFieldEmail newEmail)
        Right (_, usr) <- runQuery conn $ lookupUser userId
        usr `shouldBe` (user & userEmail .~ newEmail)

    it "changes the user name" $ \(ActionState (conn, _, _)) -> do
        let newPass = encryptTestSecret "new pass"
        Right _ <- runQuery conn $ addUserPrim userId user
        Right _ <- runQuery conn $
            updateUserField userId (UpdateUserFieldPassword newPass)
        Right (_, usr) <- runQuery conn $ lookupUser userId
        usr `shouldBe` (user & userPassword .~ newPass)

    it "doesn't change other users" $ \(ActionState (conn, _, _)) -> do
        let newName = UserName "new"
        Right _ <- runQuery conn $ addUserPrim userId user
        Right _ <- runQuery conn $ addUserPrim user2Id user2
        Right _ <- runQuery conn $
            updateUserField userId (UpdateUserFieldName newName)
        Right (_, usr2) <- runQuery conn $ lookupUser user2Id
        usr2 `shouldBe` user2

    it "fails if the new name is not unique" $ \(ActionState (conn, _, _)) -> do
        let newName = user2 ^. userName
        Right _ <- runQuery conn $ addUserPrim userId user
        Right _ <- runQuery conn $ addUserPrim user2Id user2
        x <- runQuery conn $ updateUserField userId (UpdateUserFieldName newName)
        x `shouldBe` Left UserNameAlreadyExists
        runQuery conn (lookupUser userId) `shouldReturn` Right (userId, user)
        runQuery conn (lookupUser user2Id) `shouldReturn` Right (user2Id, user2)

    it "fails if the user doesn't exist" $ \(ActionState (conn, _, _)) -> do
        x <- runQuery conn $ updateUserField userId $ UpdateUserFieldName $ UserName "nobody"
        x `shouldBe` Left NoSuchUser

agentRolesSpec :: SpecWith ActionState
agentRolesSpec = describe "agentRoles" $ do
    it "returns an empty set for a user without roles" $ \(ActionState (conn, _, _)) -> do
        let user = mkUser "name" "super secret" "me@example.com"
            userId = UserId 111
        Right _ <- runQuery conn $ addUserPrim userId user
        x <- runQuery conn $ agentRoles (UserA userId)
        x `shouldBe` Right Set.empty

assignRoleSpec :: SpecWith ActionState
assignRoleSpec = describe "assignRole" $ do
    let user = mkUser "name" "super secret" "me@example.com"
        userId = UserId 111

    it "adds a role" $ \(ActionState (conn, _, _)) -> do
        Right _ <- runQuery conn $ addUserPrim userId user
        Right _ <- runQuery conn $ assignRole (UserA userId) RoleAdmin
        Right roles <- runQuery conn $ agentRoles (UserA userId)
        roles `shouldBe` Set.fromList [RoleAdmin]

    it "silently allows adding a duplicate role" $ \(ActionState (conn, _, _)) -> do
        Right _ <- runQuery conn $ addUserPrim userId user
        Right _ <- runQuery conn $ assignRole (UserA userId) RoleAdmin
        x <- runQuery conn $ assignRole (UserA userId) RoleAdmin
        x `shouldBe` Right ()
        Right roles <- runQuery conn $ agentRoles (UserA userId)
        roles `shouldBe` Set.fromList [RoleAdmin]

    it "adds a second role" $ \(ActionState (conn, _, _)) -> do
        Right _ <- runQuery conn $ addUserPrim userId user
        Right _ <- runQuery conn $ assignRole (UserA userId) RoleAdmin
        Right _ <- runQuery conn $ assignRole (UserA userId) RoleUser
        Right roles <- runQuery conn $ agentRoles (UserA userId)
        roles `shouldBe` Set.fromList [RoleAdmin, RoleUser]

unassignRoleSpec :: SpecWith ActionState
unassignRoleSpec = describe "unassignRole" $ do
    let user = mkUser "name" "super secret" "me@example.com"
        userId = UserId 111

    it "silently allows removing a non-assigned role" $ \(ActionState (conn, _, _)) -> do
        Right _ <- runQuery conn $ addUserPrim userId user
        x <- runQuery conn $ unassignRole (UserA userId) RoleAdmin
        x `shouldBe` Right ()

    it "removes the specified role" $ \(ActionState (conn, _, _)) -> do
        Right _ <- runQuery conn $ addUserPrim userId user
        Right _ <- runQuery conn $ assignRole (UserA userId) RoleAdmin
        Right _ <- runQuery conn $ assignRole (UserA userId) RoleUser
        Right _ <- runQuery conn $ unassignRole (UserA userId) RoleAdmin
        Right roles <- runQuery conn $ agentRoles (UserA userId)
        roles `shouldBe` Set.fromList [RoleUser]

emailChangeRequestSpec :: SpecWith ActionState
emailChangeRequestSpec = describe "addUserEmailChangeToken" $ do
    it "adds an email change token to the db" $ \(ActionState (conn, _, _)) -> do
        Right _ <- runThentosQuery conn $ addUserPrim userId user
        Right _ <- runThentosQuery conn $
            addUserEmailChangeRequest userId newEmail testToken
        [Only tokenInDb] <- query conn
            [sql| SELECT token FROM email_change_tokens|] ()
        tokenInDb `shouldBe` testToken

    it "changes a user's email if given a valid token" $ \(ActionState (conn, _, _)) -> do
        Right _ <- runThentosQuery conn $ addUserPrim userId user
        Right _ <- runThentosQuery conn $
            addUserEmailChangeRequest userId newEmail testToken
        Right _ <-
            runThentosQuery conn $ confirmUserEmailChange (Timeout 3600) testToken
        [Only expectedEmail] <- query conn
            [sql| SELECT email FROM users WHERE id = ?|] (Only userId)
        expectedEmail `shouldBe` newEmail
  where
    user = mkUser "name" "super secret" "me@example.com"
    userId = UserId 584
    testToken = ConfirmationToken "asgbagbaosubgoas"
    newEmail = forceUserEmail "new@example.com"

addServiceSpec :: SpecWith ActionState
addServiceSpec = describe "addService" $ do
    it "adds a service to the db" $ \(ActionState (conn, _, _)) -> do
        Right _ <- runThentosQuery conn $ addUserPrim uid user
        [Only serviceCount] <- query conn countServices ()
        serviceCount `shouldBe` (0 :: Int)
        hashedKey <- hashServiceKey secret
        Right _ <- runThentosQuery conn $
            addService (UserA uid) sid hashedKey name description
        [(owner', sid', key', name', desc')] <- query conn
            [sql| SELECT owner, id, key, name, description
                  FROM services |] ()
        owner' `shouldBe` UserA uid
        sid' `shouldBe` sid
        key' `shouldBe` hashedKey
        name' `shouldBe` name
        desc' `shouldBe` description
  where
    secret = "verySecretKey"
    sid = ServiceId "serviceid1"
    uid = UserId 9
    user = mkUser "name" "super secret" "me@example.com"
    name = ServiceName "MyLittleService"
    description = ServiceDescription "it serves"
    countServices = [sql| SELECT COUNT(*) FROM services |]

deleteServiceSpec :: SpecWith ActionState
deleteServiceSpec = describe "deleteService" $ do
    it "deletes a service" $ \(ActionState (conn, _, _)) -> do
        Right _ <- runThentosQuery conn $ addUserPrim testUid testUser
        Right _ <- runThentosQuery conn $
            addService (UserA testUid) sid testHashedSecret "" ""
        Right _ <- runThentosQuery conn $ deleteService sid
        [Only serviceCount] <- query conn [sql| SELECT COUNT(*) FROM services |] ()
        serviceCount `shouldBe` (0 :: Int)

    it "fails if the service doesn't exist" $ \(ActionState (conn, _, _)) -> do
        Left NoSuchService <- runThentosQuery conn $ deleteService sid
        return ()
  where
    sid = ServiceId "blablabla"


-- * Garbage collection

doGarbageCollectUnconfirmedUsersSpec :: SpecWith ActionState
doGarbageCollectUnconfirmedUsersSpec = describe "doGarbageCollectUnconfirmedUsers" $ do
    let user1   = mkUser "name1" "pass" "email1@email.com"
        userid1 = UserId 321
        token1  = "sometoken1"
        user2   = mkUser "name2" "pass" "email2@email.com"
        userid2 = UserId 322
        token2  = "sometoken2"

    it "deletes all expired unconfirmed users" $ \ (ActionState (conn, _, _)) -> do
        Right () <- runQuery conn $ addUnconfirmedUserWithId token1 user1 userid1
        Right () <- runQuery conn $ addUnconfirmedUserWithId token2 user2 userid2
        Right () <- runQuery conn $ garbageCollectUnconfirmedUsers 0
        [Only tkns] <- query_ conn [sql| SELECT count(*) FROM user_confirmation_tokens |]
        [Only usrs] <- query_ conn [sql| SELECT count(*) FROM "users" |]
        tkns `shouldBe` (0 :: Int)
        usrs `shouldBe` (0 :: Int)

    it "only deletes expired unconfirmed users" $ \ (ActionState (conn, _, _)) -> do
        Right () <- runQuery conn $ addUnconfirmedUserWithId token1 user1 userid1
        Right () <- runQuery conn $ garbageCollectUnconfirmedUsers 100000
        [Only tkns] <- query_ conn [sql| SELECT count(*) FROM user_confirmation_tokens |]
        [Only usrs] <- query_ conn [sql| SELECT count(*) FROM "users" |]
        tkns `shouldBe` (1 :: Int)
        usrs `shouldBe` (1 :: Int)


garbageCollectPasswordResetTokensSpec :: SpecWith ActionState
garbageCollectPasswordResetTokensSpec = describe "garbageCollectPasswordResetTokens" $ do
    let user   = mkUser "name1" "pass" "email1@email.com"
        userid = UserId 321
        email = forceUserEmail "email1@email.com"
        passToken = "sometoken2"

    it "deletes all expired tokens" $ \ (ActionState (conn, _, _)) -> do
        void $ runQuery conn $ addUserPrim userid user
        void $ runQuery conn $ addPasswordResetToken email passToken
        [Only tkns] <- query_ conn [sql| SELECT count(*) FROM password_reset_tokens |]
        tkns `shouldBe` (1 :: Int)
        Right () <- runQuery conn $ garbageCollectPasswordResetTokens 0
        [Only tkns'] <- query_ conn [sql| SELECT count(*) FROM password_reset_tokens |]
        tkns' `shouldBe` (0 :: Int)

    it "only deletes expired tokens" $ \ (ActionState (conn, _, _)) -> do
        void $ runQuery conn $ addUserPrim userid user
        void $ runQuery conn $ addPasswordResetToken email passToken
        void $ runQuery conn $ garbageCollectPasswordResetTokens 1000000
        [Only tkns'] <- query_ conn [sql| SELECT count(*) FROM password_reset_tokens |]
        tkns' `shouldBe` (1 :: Int)

startThentosSessionSpec :: SpecWith ActionState
startThentosSessionSpec = describe "startThentosSession" $ do
    let tok = "something"
        user = UserA (UserId 55)
        period = Timeout $ fromSeconds' 60

    it "fails when the user doesn't exist" $ \(ActionState (conn, _, _)) -> do
        x <- runQuery conn $ startThentosSession tok user period
        x `shouldBe` Left NoSuchUser

lookupThentosSessionSpec :: SpecWith ActionState
lookupThentosSessionSpec = describe "lookupThentosSession" $ do
    let tok = ThentosSessionToken "hellohello"
        userId = UserId 777
        user = mkUser "name" "pass" "email@email.com"
        agent = UserA userId
        period' = fromSeconds' 60
        period = Timeout period'

    it "fails when there is no session" $ \(ActionState (conn, _, _)) -> do
        void $ runQuery conn $ addUserPrim userId user
        x <- runQuery conn $ lookupThentosSession tok
        x `shouldBe` Left NoSuchThentosSession

    it "reads back a fresh session" $ \(ActionState (conn, _, _)) -> do
        void $ runQuery conn $ addUserPrim userId user
        Right _ <- runQuery conn $ startThentosSession tok agent period
        Right (t, s) <- runQuery conn $ lookupThentosSession tok
        t `shouldBe` tok
        s ^. thSessAgent `shouldBe` agent

    it "fails for an expired session" $ \(ActionState (conn, _, _)) -> do
        void $ runQuery conn $ addUserPrim userId user
        Right _ <- runQuery conn $ startThentosSession tok agent (Timeout $ fromSeconds' 0)
        x <- runQuery conn $ lookupThentosSession tok
        x `shouldBe` Left NoSuchThentosSession

    it "extends the session" $ \(ActionState (conn, _, _)) -> do
        void $ runQuery conn $ addUserPrim userId user
        Right _ <- runQuery conn $ startThentosSession tok agent period
        Right (_, sess1) <- runQuery conn $ lookupThentosSession tok
        Right (t, sess2) <- runQuery conn $ lookupThentosSession tok
        t `shouldBe` tok
        sess1 ^. thSessEnd `shouldSatisfy` (< sess2 ^. thSessEnd)

endThentosSessionSpec :: SpecWith ActionState
endThentosSessionSpec = describe "endThentosSession" $ do
    let tok = "something"
        userId = UserId 777
        user = mkUser "name" "pass" "email@email.com"
        agent = UserA userId
        period = Timeout $ fromSeconds' 60

    it "deletes a session" $ \(ActionState (conn, _, _)) -> do
        void $ runQuery conn $ addUserPrim userId user
        void $ runQuery conn $ startThentosSession tok agent period
        Right _ <- runQuery conn $ lookupThentosSession tok
        Right _ <- runQuery conn $ endThentosSession tok
        x <- runQuery conn $ lookupThentosSession tok
        x `shouldBe` Left NoSuchThentosSession

    it "silently allows deleting a non-existing session" $ \(ActionState (conn, _, _)) -> do
        x <- runQuery conn $ endThentosSession tok
        x `shouldSatisfy` isRight

startServiceSessionSpec :: SpecWith ActionState
startServiceSessionSpec = describe "startServiceSession" $ do
    it "starts a service session" $ \(ActionState (conn, _, _)) -> do
        void $ runQuery conn $ addUserPrim testUid testUser
        void $ runQuery conn $
            startThentosSession thentosSessionToken (UserA testUid) period
        void $ runQuery conn $
            addService (UserA testUid) sid testHashedSecret "" ""
        void $ runQuery conn $
            startServiceSession thentosSessionToken serviceSessionToken sid period
        [Only count] <- query_ conn [sql| SELECT COUNT(*) FROM service_sessions |]
        count `shouldBe` (1 :: Int)
  where
    period = Timeout $ fromSeconds' 60
    thentosSessionToken = "foo"
    serviceSessionToken = "bar"
    sid = "sid"

endServiceSessionSpec :: SpecWith ActionState
endServiceSessionSpec = describe "endServiceSession" $ do
    it "ends an service session" $ \(ActionState  (conn, _, _)) -> do
        void $ runQuery conn $ addUserPrim testUid testUser
        void $ runQuery conn $
            startThentosSession thentosSessionToken (UserA testUid) period
        void $ runQuery conn $
            addService (UserA testUid) sid testHashedSecret "" ""
        void $ runQuery conn $
            startServiceSession thentosSessionToken serviceSessionToken sid period
        [Only count] <- query_ conn countSessions
        count `shouldBe` (1 :: Int)
        void $ runQuery conn $ endServiceSession serviceSessionToken
        [Only count'] <- query_ conn countSessions
        count' `shouldBe` (0 :: Int)
        return ()
  where
    countSessions = [sql| SELECT COUNT(*) FROM service_sessions |]
    period = Timeout $ fromSeconds' 60
    thentosSessionToken = "foo"
    serviceSessionToken = "bar"
    sid = "sid"

garbageCollectEmailChangeTokensSpec :: SpecWith ActionState
garbageCollectEmailChangeTokensSpec = describe "doGarbageCollectPasswordResetTokens" $ do
    let newEmail = forceUserEmail "new@example.com"
        token = "sometoken2"

    it "deletes all expired tokens" $ \ (ActionState (conn, _, _)) -> do
        Right _ <- runQuery conn $ addUserPrim testUid testUser
        Right _ <- runQuery conn $ addUserEmailChangeRequest testUid newEmail token
        [Only tokenCount] <- query_ conn [sql| SELECT count(*) FROM email_change_tokens |]
        tokenCount `shouldBe` (1 :: Int)
        Right () <- runQuery conn $ garbageCollectEmailChangeTokens 0
        [Only tokenCount'] <- query_ conn [sql| SELECT count(*) FROM email_change_tokens |]
        tokenCount' `shouldBe` (0 :: Int)

    it "only deletes expired tokens" $ \ (ActionState (conn, _, _)) -> do
        Right _ <- runQuery conn $ addUserPrim testUid testUser
        Right _ <- runQuery conn $ addUserEmailChangeRequest testUid newEmail token
        Right () <- runQuery conn $ garbageCollectEmailChangeTokens 1000000
        [Only tkns'] <- query_ conn [sql| SELECT count(*) FROM email_change_tokens |]
        tkns' `shouldBe` (1 :: Int)


-- * Utils


mkUser :: UserName -> SBS -> ST -> User
mkUser name pass email = User { _userName = name
                              , _userPassword = encryptTestSecret pass
                              , _userEmail = forceUserEmail email
                              , _userThentosSessions = mempty
                              , _userServices = mempty
                              }


-- specialize error type to Void
runQuery :: Connection -> ThentosQuery Void a -> IO (Either (ThentosError Void) a)
runQuery = runThentosQuery

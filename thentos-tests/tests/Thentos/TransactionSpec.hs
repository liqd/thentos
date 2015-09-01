{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Thentos.TransactionSpec (spec) where

import Control.Applicative ((<$>))
import Data.Monoid (mempty)
import Control.Lens ((&), (^.), (.~))
import Control.Monad (void)
import Data.String.Conversions (ST, SBS)
import Data.Void (Void)
import Database.PostgreSQL.Simple (Only(..), query, query_)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Test.Hspec (Spec, SpecWith, describe, it, shouldBe, shouldReturn, before)
import Database.PostgreSQL.Simple (Connection)

import Thentos.Action.Core
import Thentos.Transaction
import Thentos.Transaction.Core
import Thentos.Types
import Thentos.Util (hashUserPass)

import Thentos.Test.Core
import Thentos.Test.Config

spec :: Spec
spec = describe "Thentos.Transaction" . before (createActionState thentosTestConfig) $ do
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
    doGarbageCollectUnconfirmedUsersSpec
    doGarbageCollectPasswordResetTokensSpec

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
        [res] <- query conn [sql|
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
        res `shouldBe` ([] :: [ConfirmationToken])

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

    it "resets a password when the given token exists" $ \(ActionState (conn, _, _)) -> do
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
        Right () <- runQuery conn $ doGarbageCollectUnconfirmedUsers 0
        [Only tkns] <- query_ conn [sql| SELECT count(*) FROM user_confirmation_tokens |]
        [Only usrs] <- query_ conn [sql| SELECT count(*) FROM "users" |]
        tkns `shouldBe` (0 :: Int)
        usrs `shouldBe` (0 :: Int)

    it "only deletes expired unconfirmed users" $ \ (ActionState (conn, _, _)) -> do
        Right () <- runQuery conn $ addUnconfirmedUserWithId token1 user1 userid1
        Right () <- runQuery conn $ doGarbageCollectUnconfirmedUsers 100000
        [Only tkns] <- query_ conn [sql| SELECT count(*) FROM user_confirmation_tokens |]
        [Only usrs] <- query_ conn [sql| SELECT count(*) FROM "users" |]
        tkns `shouldBe` (1 :: Int)
        usrs `shouldBe` (1 :: Int)


doGarbageCollectPasswordResetTokensSpec :: SpecWith ActionState
doGarbageCollectPasswordResetTokensSpec = describe "doGarbageCollectPasswordResetTokens" $ do
    let user   = mkUser "name1" "pass" "email1@email.com"
        userid = UserId 321
        email = forceUserEmail "email1@email.com"
        passToken = "sometoken2"

    it "deletes all expired tokens" $ \ (ActionState (conn, _, _)) -> do
        void $ runQuery conn $ addUserPrim userid user
        void $ runQuery conn $ addPasswordResetToken email passToken
        [Only tkns] <- query_ conn [sql| SELECT count(*) FROM password_reset_tokens |]
        tkns `shouldBe` (1 :: Int)
        Right () <- runQuery conn $ doGarbageCollectPasswordResetTokens 0
        [Only tkns'] <- query_ conn [sql| SELECT count(*) FROM password_reset_tokens |]
        tkns' `shouldBe` (0 :: Int)

    it "only deletes expired tokens" $ \ (ActionState (conn, _, _)) -> do
        void $ runQuery conn $ addUserPrim userid user
        void $ runQuery conn $ addPasswordResetToken email passToken
        void $ runQuery conn $ doGarbageCollectPasswordResetTokens 1000000
        [Only tkns'] <- query_ conn [sql| SELECT count(*) FROM password_reset_tokens |]
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

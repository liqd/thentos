{-# LANGUAGE OverloadedStrings #-}
module Thentos.TransactionSpec (spec) where

import Data.Monoid (mempty)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (ST, SBS)
import Data.Void (Void)
import Test.Hspec (Spec, SpecWith, describe, it, shouldBe, shouldReturn, before)
import Database.PostgreSQL.Simple (Connection)

import Thentos.Action.Core
import Thentos.Transaction
import Thentos.Transaction.Core
import Thentos.Types

import Thentos.Test.Core
import Thentos.Test.Config

spec :: Spec
spec = describe "Thentos.Transaction" . before (createActionState thentosTestConfig) $ do
    addUserPrimSpec
    lookupUserByNameSpec
    lookupUserByEmailSpec

addUserPrimSpec :: SpecWith ActionState
addUserPrimSpec = describe "addUserPrim" $ do

    it "adds a user to the database" $ \ (ActionState (conn, _, _)) -> do
        let user   = testUsers !! 2
            userId = UserId 289
        void $ runQuery conn $ addUserPrim userId user
        Right (_, res) <- runQuery conn $ lookupUser userId
        liftIO $ res `shouldBe` user

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

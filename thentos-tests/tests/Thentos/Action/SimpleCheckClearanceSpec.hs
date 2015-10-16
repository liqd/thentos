{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Thentos.Action.SimpleCheckClearanceSpec where

import Control.Exception (SomeException)
import Data.Pool (withResource)
import Data.Void (Void)
import LIO.DCLabel (toCNF)
import Test.Hspec (Spec, SpecWith, describe, it, before, shouldThrow, hspec)

import Thentos.Action.Core
import Thentos.Action.SimpleCheckClearance
import Thentos.Types

import Thentos.Test.Arbitrary ()
import Thentos.Test.Config
import Thentos.Test.Core


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = do
    let mkActionState = do
          actionState@(ActionState (connPool, _, _)) <- createActionState "test_thentos" thentosTestConfig
          withResource connPool createGod
          return actionState

    describe "Thentos.Action" . before mkActionState $ specWithActionState

type Act = Action (ActionError Void)

setClearanceUserId :: Integer -> Action e ()
setClearanceUserId uid = grantAccessRights'P [toCNF . UserA . UserId $ uid, toCNF RoleUser]


specWithActionState :: SpecWith ActionState
specWithActionState = describe "Thentos.Action.SimpleCheckClearanceSpec" $ do
    describe "assertAuth" $ do
        it "throws an error on False" $ \sta -> do
            runAction sta (assertAuth $ pure False :: Action (ActionError Void) ())
                `shouldThrow` (\(_ :: SomeException) -> True)
        it "returns () on True" $ \sta -> do
            runAction sta (assertAuth $ pure True :: Action (ActionError Void) ())

    describe "isUserLoggedIn" $ do
        it "returns True if 'RoleUser' is present" $ \sta -> do
            True <- runAction sta (setClearanceUserId 3 >> isUserLoggedIn :: Act Bool)
            True <- runAction sta (setClearanceUserId 5 >> isUserLoggedIn :: Act Bool)
            return ()
        it "returns False if 'RoleUser' is missing" $ \sta -> do
            False <- runAction sta (isUserLoggedIn :: Act Bool)
            return ()

    describe "doesUserHaveId" $ do
        it "returns True on if uid matches" $ \sta -> do
            True <- runAction sta (setClearanceUserId 3 >> doesUserHaveId (UserId 3) :: Act Bool)
            return ()
        it "returns False on if uid does not match" $ \sta -> do
            False <- runAction sta (doesUserHaveId (UserId 3) :: Act Bool)
            False <- runAction sta (setClearanceUserId 5 >> doesUserHaveId (UserId 3) :: Act Bool)
            return ()

    describe "guardedUnsafeAction" $ do
        it "runs unsafe action if predicate is satisfied" $ \sta -> do
            3 <- runAction sta (guardedUnsafeAction (pure True) (pure 3) :: Act Int)
            return ()
        it "throws an error otherwise" $ \sta -> do
            runAction sta (guardedUnsafeAction (pure False) (pure 3) :: Act Int)
                `shouldThrow` (\(_ :: SomeException) -> True)

    describe "unsafeAction" $ do
        it "translates an UnsafeAction into an Action, unsafely" $ \sta -> do
            4 <- runAction sta (unsafeAction (pure 4) :: Act Int)
            return ()

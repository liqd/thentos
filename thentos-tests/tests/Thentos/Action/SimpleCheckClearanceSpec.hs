{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Thentos.Action.SimpleCheckClearanceSpec where

import Data.Pool (withResource)
import Data.String.Conversions (cs)
import Data.Void (Void)
import LIO.DCLabel (toCNF)
import Test.Hspec (Spec, SpecWith, describe, it, before, hspec)

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

    describe "Thentos.Action.SimpleCheckClearance" . before mkActionState $ specWithActionState

type Act = Action (ActionError Void)

setTwoRoles :: Action e ()
setTwoRoles = grantAccessRights'P [RoleAdmin, RoleUser]

setClearanceUid :: Integer -> Action e ()
setClearanceUid uid = grantAccessRights'P [toCNF . UserA . UserId $ uid, toCNF RoleUser]

setClearanceSid :: Integer -> Action e ()
setClearanceSid sid = grantAccessRights'P [toCNF . ServiceA . ServiceId . cs . show $ sid]


specWithActionState :: SpecWith ActionState
specWithActionState = do
    describe "assertAuth" $ do
        it "throws an error on False" $ \sta -> do
            Left (ActionErrorAnyLabel _)
                <- runActionE sta (assertAuth $ pure False :: Action (ActionError Void) ())
            return ()
        it "returns () on True" $ \sta -> do
            runAction sta (assertAuth $ pure True :: Action (ActionError Void) ())

    describe "hasUserId" $ do
        it "returns True on if uid matches" $ \sta -> do
            True <- runAction sta (setClearanceUid 3 >> hasUserId (UserId 3) :: Act Bool)
            return ()
        it "returns False on if uid does not match" $ \sta -> do
            False <- runAction sta (hasUserId (UserId 3) :: Act Bool)
            False <- runAction sta (setClearanceUid 5 >> hasUserId (UserId 3) :: Act Bool)
            return ()

    describe "hasServiceId" $ do
        it "returns True on if sid matches" $ \sta -> do
            True <- runAction sta (setClearanceSid 3 >> hasServiceId (ServiceId "3") :: Act Bool)
            return ()
        it "returns False on if sid does not match" $ \sta -> do
            False <- runAction sta (hasServiceId (ServiceId "3") :: Act Bool)
            False <- runAction sta (setClearanceSid 5 >> hasServiceId (ServiceId "3") :: Act Bool)
            return ()
        it "can distinguish uid and sid" $ \sta -> do
            False <- runAction sta (setClearanceUid 3 >> hasServiceId (ServiceId "3") :: Act Bool)
            return ()

    describe "hasRole" $ do
        it "returns True if role is present" $ \sta -> do
            True <- runAction sta (setClearanceUid 3 >> hasRole RoleUser :: Act Bool)
            True <- runAction sta (setClearanceUid 5 >> hasRole RoleUser :: Act Bool)
            True <- runAction sta (setTwoRoles >> hasRole RoleUser :: Act Bool)
            return ()
        it "returns False if role is missing" $ \sta -> do
            False <- runAction sta (hasRole RoleUser :: Act Bool)
            False <- runAction sta (setTwoRoles >> hasRole RoleServiceAdmin :: Act Bool)
            return ()

    describe "guardedUnsafeAction" $ do
        it "runs unsafe action if predicate is satisfied" $ \sta -> do
            3 <- runAction sta (guardedUnsafeAction (pure True) (pure 3) :: Act Int)
            return ()
        it "throws an error otherwise" $ \sta -> do
            Left (ActionErrorAnyLabel _)
                <- runActionE sta (guardedUnsafeAction (pure False) (pure 3) :: Act Int)
            return ()

    describe "unsafeAction" $ do
        it "translates an UnsafeAction into an Action, unsafely" $ \sta -> do
            4 <- runAction sta (unsafeAction (pure 4) :: Act Int)
            return ()

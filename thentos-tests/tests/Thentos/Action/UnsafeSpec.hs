{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -Wall -fwarn-unused-imports #-}

module Thentos.Action.UnsafeSpec where

import Data.Pool (withResource)
import Data.String.Conversions (cs)
import Data.Void (Void)
import Test.Hspec (Spec, describe, it, before, hspec)

import Thentos.Action.Core
import Thentos.Action.Unsafe
import Thentos.Types

import Thentos.Test.Config
import Thentos.Test.Core


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = specWithActionState

type Act = Action (ActionError Void) ()


mkActionState :: IO ActionState
mkActionState = do
    actionState@(ActionState (connPool, _, _)) <- createActionState "test_thentos" thentosTestConfig
    withResource connPool createGod
    return actionState

specWithActionState :: Spec
specWithActionState = before mkActionState $ do
    describe "guardedUnsafeAction" $ do
        it "runs unsafe action if predicate is satisfied" $ \sta -> do
            (3, ()) <- runAction () sta (guardedUnsafeAction (pure True) (pure 3) :: Act Int)
            return ()
        it "throws an error otherwise" $ \sta -> do
            (Left (ActionErrorAnyLabel _), ())
                <- runActionE () sta (guardedUnsafeAction (pure False) (pure 3) :: Act Int)
            return ()

    describe "unsafeAction" $ do
        it "translates an UnsafeAction into an Action, unsafely" $ \sta -> do
            (4, ()) <- runAction () sta (unsafeAction (pure 4) :: Act Int)
            return ()

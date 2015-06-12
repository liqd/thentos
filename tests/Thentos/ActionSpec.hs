{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Thentos.ActionSpec where

import Test.Hspec (Spec, SpecWith, describe, it, before, after, shouldBe, hspec)

import LIO.Missing
import Thentos.Action
import Thentos.Action.Core
import Thentos.Types

import Test.Arbitrary ()
import Test.Core
import Test.Types


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "Thentos.Action" . before setupDB . after teardownDB $ do
    spec_existsThentosSession

spec_existsThentosSession :: SpecWith DBTS
spec_existsThentosSession = describe "lookupThentosSession user" $ do
  it "works" $ \ (DBTS _ astate :: DBTS) -> do
    ((ernieId, ernieF, _) : (bertId, _, _) : _)
        <- runActionWithClearance dcTop astate initializeTestUsers

    tok <- runActionWithClearance dcTop astate $
            startThentosSessionByUserId ernieId (udPassword ernieF)
    v1 <- runActionAsAgent (UserA ernieId) astate (existsThentosSession tok)
    v2 <- runActionAsAgent (UserA bertId)  astate (existsThentosSession tok)

    runActionWithClearance dcTop astate $ endThentosSession tok
    v3 <- runActionAsAgent (UserA ernieId) astate (existsThentosSession tok)
    v4 <- runActionAsAgent (UserA bertId)  astate (existsThentosSession tok)

    (v1, v2, v3, v4) `shouldBe` (True, False, False, False)

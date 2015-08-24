module Thentos.TransactionSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Test.Hspec (Spec, SpecWith, describe, it, shouldBe, before)

import Thentos.Action.Core
import Thentos.Transaction
import Thentos.Transaction.Core
import Thentos.Types

import Thentos.Test.Core
import Thentos.Test.Config

spec :: Spec
spec = describe "Thentos.Transaction" . before (createActionState thentosTestConfig) $ do
    addUserPrimSpec

addUserPrimSpec :: SpecWith ActionState
addUserPrimSpec = describe "addUserPrim" $ do

    it "adds a user to the database" $ \ (ActionState (conn, _, _)) -> do
        let user   = testUsers !! 2
            userId = UserId 289
        runThentosUpdate conn $ addUserPrim userId user
        Right (_, res) <- runThentosQuery conn $ lookupUser userId
        liftIO $ res `shouldBe` user

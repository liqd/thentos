{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Thentos.Frontend.Handlers.CombinatorsSpec where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.CaseInsensitive (mk)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Data.Proxy
import Data.String.Conversions (ST, cs)
import Network.Wai
import Network.Wai.Test (SResponse, simpleBody, simpleHeaders, simpleStatus)
import Servant.API
import Servant.API.ContentTypes
import Servant.HTML.Blaze
import Servant.Server
import Test.Hspec (Spec, SpecWith, around, describe, it, shouldBe, shouldContain, shouldSatisfy, hspec, pending)
import Test.Hspec.Wai

import qualified Data.Text as ST
import qualified Network.HTTP.Types.Status as C
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Class as WD
import qualified Text.Blaze.Html5 as H

import Thentos.Action.Core
import Thentos.Config
import Thentos.Frontend.Handlers.Combinators
import Thentos.Frontend.State
import Thentos.Frontend.Types
import Thentos.Types
import Thentos.Util

import qualified Thentos.Transaction as T

import Thentos.Test.Arbitrary ()
import Thentos.Test.Config
import Thentos.Test.Core
import Thentos.Test.Transaction
import Thentos.Test.WebDriver.Missing as WD


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "Thentos.Frontend.Handlers.CombinatorsSpec" $ do
    specRedirect


type ApiRedirect = "here" :> Get '[HTML] H.Html

apiRedirect :: ServerT ApiRedirect FAction
apiRedirect = redirect' "/there"

appRedirect :: IO Application
appRedirect =
    createActionState "thentos_test" thentosTestConfig >>=
    serveFAction (Proxy :: Proxy ApiRedirect) apiRedirect

specRedirect :: Spec
specRedirect = do
    describe "redirect'" . with appRedirect $ do
        it "gets you there" $ do
            resp <- request "GET" "/here" [] ""
            liftIO $ C.statusCode (simpleStatus resp) `shouldBe` 303
            liftIO $ simpleHeaders resp `shouldContain` [(mk "Location", "/there")]

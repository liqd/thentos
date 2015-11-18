{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Thentos.Frontend.Handlers.CombinatorsSpec where

import Data.Proxy (Proxy(Proxy))
import Network.Wai  (Application)
import Network.Wai.Test (simpleHeaders, simpleStatus)
import Servant.API ((:>), Get)
import Servant.HTML.Blaze (HTML)
import Servant.Server (ServerT)
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain, hspec)
import Test.Hspec.Wai (with, request, liftIO)

import qualified Network.HTTP.Types.Status as C
import qualified Text.Blaze.Html5 as H

import Thentos.Frontend.Handlers.Combinators
import Thentos.Frontend.State
import Thentos.Frontend.Types

import Thentos.Test.Arbitrary ()
import Thentos.Test.Config
import Thentos.Test.Core


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
            liftIO $ simpleHeaders resp `shouldContain` [("Location", "/there")]

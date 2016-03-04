{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Thentos.Frontend.Handlers.CombinatorsSpec where

import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Servant.API ((:>), Get)
import Servant.HTML.Blaze (HTML)
import Servant.Server (ServerT)
import Test.Hspec (Spec, describe, it, hspec)
import Test.Hspec.Wai (with, get, shouldRespondWith, matchHeaders, (<:>))

import qualified Text.Blaze.Html5 as H

import Thentos.Frontend.Handlers.Combinators
import Thentos.Frontend.State
import Thentos.Frontend.Types

import Thentos.Test.Arbitrary ()
import Thentos.Test.Core


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "Thentos.Frontend.Handlers.CombinatorsSpec" specRedirect


type ApiRedirect = "here" :> Get '[HTML] H.Html

apiRedirect :: FormHandler (ServerT ApiRedirect)
apiRedirect = redirect' "/there"

appRedirect :: IO Application
appRedirect = createActionEnv >>= serveFAction (Proxy :: Proxy ApiRedirect) apiRedirect

specRedirect :: Spec
specRedirect =
    describe "redirect'" . with appRedirect $
        it "gets you there" $
            get "/here" `shouldRespondWith` 303 { matchHeaders = ["Location" <:> "/there"] }

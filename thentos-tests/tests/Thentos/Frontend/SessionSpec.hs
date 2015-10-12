{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Thentos.Frontend.SessionSpec (tests, spec) where

import Data.Text (pack, Text)
import Data.String.Conversions (cs)
import Data.Maybe (isJust)
import Test.Hspec (Spec, describe, it, shouldSatisfy, hspec, context)
import Test.Hspec.Wai
import Network.Wai
import Network.Wai.Test (simpleBody, simpleHeaders)
import Servant
import Network.HTTP.Types
import System.IO.Unsafe


import Thentos.Frontend.Session
import Data.Aeson

tests :: IO ()
tests = hspec spec

type API = Session :> Get '[PlainText] Text

server :: Application
server = serve (Proxy :: Proxy API) handler
  where
    handler :: Server API
    handler = return . cs . unCookie

spec :: Spec
spec = describe "asdf-session-tests" . with (return server) $ do

    context "the cookie is set" $ do

        it "gets the token from the client cookie" $ do
            request methodGet "" [("Cookie", "bla")] "" `shouldRespondWith` "\"bla\""

    context "no cookie is set" $ do

        it "returns a fresh one" $ do
            resp <- request methodGet "" [] ""
            liftIO $ print (simpleBody resp)
            liftIO $ simpleBody resp `shouldSatisfy` (\(Just (Cookie _)) -> True)  -- fails because of missing leniency

        it "one will be in the Set-Cookie header of the response" $ do
            resp <- request methodGet "" [] ""
            liftIO $ simpleHeaders resp `shouldSatisfy` isJust . lookup "Set-Cookie"

    it "cookies are fresh" $ do
        pending

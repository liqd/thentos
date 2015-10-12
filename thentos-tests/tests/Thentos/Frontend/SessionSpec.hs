{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Thentos.Frontend.SessionSpec where

import Data.Maybe (isJust)
import Test.Hspec (Spec, describe, it, shouldSatisfy, hspec)
import Test.Hspec.Wai
import Network.Wai
import Network.Wai.Test (simpleBody, simpleHeaders)
import Servant
import Network.HTTP.Types


import Thentos.Frontend.Session
import Data.Aeson

tests :: IO ()
tests = hspec spec

server1 :: Application
server1 = serve (Proxy :: Proxy (Session :> Get '[JSON] Token)) return

spec :: Spec
spec = describe "asdf-session-tests" . with (return server1) $ do

    context "the cookie is set" $ do

        it "gets the token from the client cookie" $ do
            request methodGet "" [("Cookie", "bla")] "" `shouldRespondWith` "\"bla\""

    context "no cookie is set" $ do

        it "returns a fresh one" $ do
            resp <- request methodGet "" [] ""
            liftIO $ print (simpleBody resp)
            liftIO $ (decode $ simpleBody resp) `shouldSatisfy` (\(Just (Token _)) -> True)  -- fails because of missing leniency

        it "one will be in the Set-Cookie header of the response" $ do
            resp <- request methodGet "" [] ""
            liftIO $ simpleHeaders resp `shouldSatisfy` isJust . lookup "Set-Cookie"

    it "cookies are fresh" $ do
        pending

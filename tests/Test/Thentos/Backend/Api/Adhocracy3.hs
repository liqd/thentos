{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS  #-}

module Test.Thentos.Backend.Api.Adhocracy3
where

import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Crypto.Random (SystemRNG)
import Data.Acid.Advanced (query')
import Data.String.Conversions (cs, (<>))
import Network.Mail.Mime (Address(Address))
import Network.Wai (Application)
import Network.Wai.Test (srequest, simpleStatus, simpleBody)
import Test.Hspec (Spec, describe, it, before, after, shouldBe, shouldSatisfy, pendingWith)
import Test.QuickCheck (property)

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as ST
import qualified Network.HTTP.Types.Status as C

import Thentos.Api
import Thentos.Backend.Api.Adhocracy3
import Thentos.Config
import Thentos.DB
import Thentos.Types

import Test.Arbitrary ()
import Test.Util


setupTestA3Server :: IO (ActionStateGlobal (MVar SystemRNG), Application)
setupTestA3Server = do
  (st, rng, _) <- setupDB
  let asg = (st, rng,) $ ThentosConfig
        { emailSender = Address (Just "Thentos") "thentos@thentos.org"
        , frontendConfig = Just FrontendConfig { frontendPort = 7082 }
        , backendConfig = Just BackendConfig { backendPort = 7081 }
        , proxyConfig = Nothing
        , defaultUser = Nothing
        }
  return (asg, Thentos.Backend.Api.Adhocracy3.serveApi asg)


tests :: Spec
tests = do
    describe "Thentos.Backend.Api.Adhocracy3" $ do
        describe "A3UserNoPass" $ do
            it "has invertible *JSON instances" . property $
                let (===) (Right (A3UserNoPass (UserFormData n "" e)))
                          (Right (A3UserNoPass (UserFormData n' _ e')))
                              = n == n' && e == e'
                    (===) _ _ = False
                in \ (A3UserNoPass -> u) -> (Aeson.eitherDecode . Aeson.encode) u === Right u
        describe "A3UserWithPassword" $ do
            it "has invertible *JSON instances" . property $
                let (===) (Right (A3UserWithPass (UserFormData n "[password hidden]" e)))
                          (Right (A3UserWithPass (UserFormData n' _ e')))
                              = n == n' && e == e'
                    (===) _ _ = False
                in \ (A3UserWithPass -> u) -> (Aeson.eitherDecode . Aeson.encode) u === Right u

        describe "create user" . before setupTestA3Server . after teardownTestServer $ do
            it "works" $
                \ ((st, _, _), testServer) -> (debugRunSession True testServer) $ do

                    -- Aeson.encode would strip the password, so we
                    -- need to do this one by hand.
                    let rq1 = cs . unlines $
                          "{" :
                          "    \"data\": {" :
                          "        \"adhocracy_core.sheets.principal.IPasswordAuthentication\": {" :
                          "            \"password\": \"passwef\"" :
                          "        }," :
                          "        \"adhocracy_core.sheets.principal.IUserBasic\": {" :
                          "            \"email\": \"wef@wef\"," :
                          "            \"name\": \"wef\"" :
                          "        }" :
                          "    }," :
                          "    \"content_type\": \"adhocracy_core.resources.principal.IUser\"" :
                          "}" :
                          []

                    rsp1 <- srequest $ makeSRequest "POST" "/principals/users" [] rq1
                    liftIO $ C.statusCode (simpleStatus rsp1) `shouldBe` 201

                    Right (db :: DB) <- query' st $ SnapShot allowEverything
                    let [(ConfirmationToken confTok, _)] = Map.toList $ db ^. dbUnconfirmedUsers

                    let rq2 = Aeson.encode . ActivationRequest . Path $ "/activate/" <> confTok
                    rsp2 <- srequest $ makeSRequest "POST" "/activate_account" [] rq2
                    liftIO $ C.statusCode (simpleStatus rsp2) `shouldBe` 201

                    let sessTok = case Aeson.eitherDecode $ simpleBody rsp2 of
                          Right (RequestSuccess _ t) -> t
                          bad -> error $ show bad

                    liftIO $ sessTok `shouldSatisfy` (not . ST.null . fromSessionToken)

                    -- we should also do something with the token.
                    -- use proxy!  (this means this test requires a3
                    -- backend to run!)

                    return ()

        describe "send email" . before setupTestA3Server . after teardownTestServer $ do
            it "works" $
                \ _ -> pendingWith "test missing."

        describe "login" . before setupTestA3Server . after teardownTestServer $ do
            it "works" $
                \ _ -> pendingWith "test missing."

                -- (we need to close the previous session, probably
                -- just by direct access to DB api because the a3 rest
                -- api does not offer logout.)

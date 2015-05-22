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

module Thentos.Backend.Api.Adhocracy3Spec
where

import Test.Hspec (Spec)
spec :: Spec
spec = return ()

{-

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Acid.Advanced (query')
import Data.String.Conversions (cs, (<>))
import Network.Wai.Test (srequest, simpleStatus, simpleBody)
import Test.Hspec (Spec, describe, it, before, after, shouldBe, shouldSatisfy, pendingWith, hspec)
import Test.QuickCheck (property)

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as ST
import qualified Network.HTTP.Types.Status as C

import Thentos.Backend.Api.Adhocracy3
import Thentos.Config
import Thentos.Types

import Test.Arbitrary ()
import Test.Core


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = do
    describe "Thentos.Backend.Api.Adhocracy3" $ do
        describe "A3UserNoPass" $ do
            it "has invertible *JSON instances" . property $
                let (===) (Right (A3UserNoPass (UserFormData n _ e)))
                          (Right (A3UserNoPass (UserFormData n' _ e')))
                              = n == n' && e == e'
                    (===) _ _ = False
                in \ (A3UserNoPass -> u) -> (Aeson.eitherDecode . Aeson.encode) u === Right u
        describe "A3UserWithPassword" $ do
            it "has invertible *JSON instances" . property $
                \ (A3UserWithPass -> u) -> (Aeson.eitherDecode . Aeson.encode) u == Right u

        describe "create user" . before (setupTestBackend RunA3) . after teardownTestBackend $ do
            it "works" $
                \ ((st, _, _), testServer, _, _) -> debugRunSession False testServer $ do

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

        describe "send email" . before (setupTestBackend RunA3) . after teardownTestBackend $ do
            it "works" $
                \ _ -> pendingWith "test missing."

        describe "login" . before (setupTestBackend RunA3) . after teardownTestBackend $ do
            it "works" $
                \ _ -> pendingWith "test missing."

                -- (we need to close the previous session, probably
                -- just by direct access to DB api because the a3 rest
                -- api does not offer logout.)

-}

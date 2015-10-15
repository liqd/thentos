{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.MissingSpec (spec) where

import Servant
import Servant.Missing

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import qualified Text.Blaze.Html as H
import qualified Data.Text as Text
import Control.Monad.IO.Class (liftIO)
import Text.Digestive
import Text.Digestive.Blaze.Html5
import Text.Blaze.Html.Renderer.Utf8
import Network.Wai.Test (simpleBody)
import Test.Hspec
import Test.Hspec.Wai (get, with, post, shouldRespondWith)


spec :: Spec
spec = describe "Servant.Missing" $ do
    formSpec


formSpec :: Spec
formSpec = describe "Forms" $ with (return $ serve api server) $ do

    context "FormGet" $ do

        it "returns the Html from 'formView'" $ do
            v <- getForm "test" personForm
            r <- get "form"
            liftIO $ simpleBody r `shouldBe` renderHtml (renderPersonForm v)

        it "responds with 200" $ do
            get "form" `shouldRespondWith` 200

    context "FormPost" $ do

        it "responds with 400 if the form is invalid" $ do
            post "form" "" `shouldRespondWith` 200

        {-it "returns the validation errors" pending-}

        {-it "accepts valid forms" pending-}



type API = "form" :> FormGet "PersonForm" H.Html Person
      :<|> "form" :> FormPost "PersonForm" H.Html Person :> Post '[JSON] Person

api :: Proxy API
api = Proxy

server :: Server API
server = return () :<|> return

data Person = Person { _name :: Text.Text, _age :: Int }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

personForm :: Monad m => Form H.Html m Person
personForm = Person <$> "name" .: nonEmptyText
                    <*> "age"  .: positiveInt
  where
    nonEmptyText = check "Cannot be empty" (not . Text.null)
                 $ text Nothing
    positiveInt  = check "Must be positive" (>= 0)
                 $ stringRead "Not a number" Nothing

renderPersonForm :: View H.Html -> H.Html
renderPersonForm v = form v "POST" $ do
    inputText "name" v
    inputText "age" v

instance HasForm "PersonForm" H.Html Person where
   isForm _      = personForm
   formView _    = renderPersonForm
   formBackend _ = error "No backend"

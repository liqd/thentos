{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Servant.MissingSpec (spec) where

import           Servant
import           Servant.Missing

import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.ByteString.Lazy.Char8    (unpack)
import qualified Data.Text                     as Text
import           GHC.Generics                  (Generic)
import           Network.Wai.Test              (simpleBody)
import           Test.Hspec
import           Test.Hspec.Wai                (get, postHtmlForm,
                                                shouldRespondWith, with)
import qualified Text.Blaze.Html               as H
import           Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html5              as H
import           Text.Digestive
import           Text.Digestive.Blaze.Html5


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
            postHtmlForm "form" [] `shouldRespondWith` 400
            postHtmlForm "form" [("test.name", "aname"), ("test.age", "-1")]
                `shouldRespondWith` 400

        it "returns the validation errors" $ do
            r1 <- postHtmlForm "form" [("test.name", ""), ("test.age", "1")]
            liftIO $ unpack (simpleBody r1) `shouldContain` "Cannot be empty"
            r2 <- postHtmlForm "form" [("test.name", "aname"), ("test.age", "-1")]
            liftIO $ unpack (simpleBody r2) `shouldContain` "Must be positive"

        it "accepts valid forms" $ do
            postHtmlForm "form" [("test.name", "aname"), ("test.age", "1")]
                `shouldRespondWith` 201



type API = "form" :> (FormGet "test" H.Html Person
      :<|> FormPost "test" H.Html Person :> Post '[JSON] Person)

api :: Proxy API
api = Proxy

server :: Server API
server = return () :<|> return

data Person = Person { name :: Text.Text, age :: Int }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

personForm :: Monad m => Form H.Html m Person
personForm = Person <$> "name" .: nonEmptyText
                    <*> "age"  .: positiveInt
  where
    nonEmptyText = check "Cannot be empty" (not . Text.null)
                 $ text Nothing
    positiveInt  = check "Must be positive" (> 0)
                 $ stringRead "Not a number" Nothing

renderPersonForm :: View H.Html -> H.Html
renderPersonForm v = form v "POST" $ do
    H.p $ do
        label "name" v "Name"
        inputText "name" v
        errorList "name" v
    H.p $ do
        label "age" v "Age"
        inputText "age" v
        errorList "age" v
    inputSubmit "submit"

instance HasForm "test" H.Html Person where
   isForm _      = personForm
   formView _    = renderPersonForm
   formBackend _ = error "No backend"
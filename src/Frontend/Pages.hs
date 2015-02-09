{-# LANGUAGE OverloadedStrings #-}

module Frontend.Pages
    ( addUserPage
    , userForm
    , userAddedPage
    , addServicePage
    , serviceAddedPage
    , loginPage
    , loginForm
    , errorPage
) where

import Control.Applicative ((<$>), (<*>))
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html (Html, (!))
import Text.Digestive.Blaze.Html5 (form, inputText, inputPassword, label, inputSubmit)
import Text.Digestive.Form (Form, check, text, (.:))
import Text.Digestive.View (View)

import Types
import Util

addUserPage :: View Html -> Html
addUserPage v = H.docTypeHtml $ do
    H.head $ do
        H.title "Create user"
    H.body $ do
        -- FIXME: how do we avoid having to duplicate the URL here?
        form v "create_user" $ do
            H.p $ do
                label "name" v "User name:"
                inputText "name" v
            H.p $ do
                label "password" v "Password:"
                inputPassword "password" v
            H.p $ do
                label "email" v "Email Address:"
                inputText "email" v
            inputSubmit "Create User"

userAddedPage :: UserId -> Html
userAddedPage uid =
    H.docTypeHtml $ do
        H.head $
            H.title "Success!"
        H.body $ do
            H.h1 "Added a user!"
            H.pre . H.string $ show uid

addServicePage :: Html
addServicePage = H.docTypeHtml $ do
    H.head $ do
        H.title "Create Service"
    H.body $ do
        H.form ! A.method "POST" ! A.action "create_service" $
            H.input ! A.type_ "submit" ! A.value "Create Service"

serviceAddedPage :: ServiceId -> ServiceKey -> Html
serviceAddedPage sid key = H.docTypeHtml $ do
    H.head $ do
        H.title "Service created!"
    H.body $ do
        H.body $ do
            H.h1 "Added a service!"
            H.p "Service id: " <> H.text (fromServiceId sid)
            H.p "Service key: " <> H.text (fromServiceKey key)

-- FIXME: move forms into separate module
userForm :: Monad m => Form Html m UserFormData
userForm = UserFormData
    <$> (UserName  <$> "name"     .: check "name must not be empty"        nonEmpty   (text Nothing))
    <*> (textToPassword <$> "password" .: check "password must not be empty"    nonEmpty   (text Nothing))
    <*> (UserEmail <$> "email"    .: check "must be a valid email address" checkEmail (text Nothing))
  where
    checkEmail :: Text -> Bool
    checkEmail = isJust . T.find (== '@')

loginPage :: View Html -> ByteString -> Html
loginPage v reqURI =
    H.docTypeHtml $ do
        H.head $
            H.title "Log in"
        H.body $
            form v (cs reqURI) $ do
                H.p $ do
                    label "usernamme" v "User name:"
                    inputText "name" v
                H.p $ do
                    label "password" v "Password:"
                    inputPassword "password" v
                inputSubmit "Log in"

loginForm :: Monad m => Form Html m (UserName, UserPass)
loginForm = (,)
    <$> (UserName  <$> "name"     .: check "name must not be empty"     nonEmpty   (text Nothing))
    <*> (textToPassword <$> "password" .: check "password must not be empty" nonEmpty   (text Nothing))

errorPage :: String -> Html
errorPage errorString = H.string $ "Encountered error: " ++ show errorString

-- auxillary functions
nonEmpty :: Text -> Bool
nonEmpty = not . T.null

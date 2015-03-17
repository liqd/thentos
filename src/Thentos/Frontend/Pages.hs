{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Thentos.Frontend.Pages
    ( mainPage
    , addUserPage
    , userForm
    , userAddedPage
    , addServicePage
    , serviceAddedPage
    , loginPage
    , loginForm
    , emailSentPage
    , errorPage
) where

import Control.Applicative ((<$>), (<*>))
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Text.Blaze.Html (Html, (!))
import Text.Digestive.Blaze.Html5 (form, inputText, inputPassword, label, inputSubmit)
import Text.Digestive.Form (Form, check, validate, text, (.:))
import Text.Digestive.Types (Result(Success, Error))
import Text.Digestive.View (View)

import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Thentos.Types

mainPage :: Html
mainPage = do
    H.head $ do
        H.title "Thentos main page"
    H.body $ do
        H.h3 $ do
            "things you can do from here:"
        H.ul $ do
            H.li . (H.a ! A.href "/login") $ "login"
            H.li . (H.a ! A.href "/create_user") $ "create_user"
            H.li . (H.a ! A.href "/signup_confirm") $ "signup_confirm"
            H.li . (H.a ! A.href "/create_service") $ "create_service"

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
                label "password1" v "Password:"
                inputPassword "password1" v
            H.p $ do
                label "password2" v "Repeat Password:"
                inputPassword "password2" v
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

userForm :: Monad m => Form Html m UserFormData
userForm = (validate validateUserData) $ (,,,)
    <$> (UserName  <$> "name"      .: check "name must not be empty"        nonEmpty   (text Nothing))
    <*> (UserPass <$> "password1"  .: check "password must not be empty"    nonEmpty   (text Nothing))
    <*> (UserPass <$> "password2"  .: check "password must not be empty"    nonEmpty   (text Nothing))
    <*> (UserEmail <$> "email"     .: check "must be a valid email address" checkEmail (text Nothing))
  where
    checkEmail :: Text -> Bool
    checkEmail = isJust . T.find (== '@')

    validateUserData (name, pw1, pw2, email)
        | pw1 == pw2 = Success $ UserFormData name pw1 email
        | otherwise  = Error "Passwords don't match"

loginPage :: ServiceId -> View Html -> ByteString -> Html
loginPage (H.string . cs . fromServiceId -> serviceId) v reqURI =
    H.docTypeHtml $ do
        H.head $
            H.title "Log in"
        H.body $ do
            H.p $ do
                "service id: " <> serviceId
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
    <*> (UserPass <$> "password" .: check "password must not be empty" nonEmpty   (text Nothing))

emailSentPage :: Html
emailSentPage = H.string $ "Please check your email"

errorPage :: String -> Html
errorPage errorString = H.string $ "Encountered error: " ++ show errorString

-- auxillary functions
nonEmpty :: Text -> Bool
nonEmpty = not . T.null

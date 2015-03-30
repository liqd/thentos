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
    , logIntoThentosPage
    , emailSentPage
    , errorPage
    , notLoggedInPage
    , requestPasswordResetPage
    , requestPasswordResetForm
    , resetPasswordPage
    , resetPasswordForm
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
            H.li . (H.a ! A.href "/log_into_thentos") $ "login"
            H.li . (H.a ! A.href "/user/create") $ "create user"
            H.li . (H.a ! A.href "/create_service") $ "create service"
            H.li . (H.a ! A.href "/user/reset_password_request") $ "request password reset"

addUserPage :: View Html -> Html
addUserPage v = H.docTypeHtml $ do
    H.head $ do
        H.title "Create user"
    H.body $ do
        -- FIXME: how do we avoid having to duplicate the URL here?
        form v "create" $ do
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
            inputSubmit "Create User" ! A.id "create_user_submit"

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
    <$> (UserName  <$> "name"    .: check "name must not be empty"     nonEmpty   (text Nothing))
    <*> (UserPass <$> "password" .: check "password must not be empty" nonEmpty   (text Nothing))

logIntoThentosPage :: View Html -> Html
logIntoThentosPage v = do
    H.docTypeHtml $ do
        H.head $
            H.title "Log into thentos"
        H.body $ do
            form v "log_into_thentos" $ do
                H.p $ do
                    label "usernamme" v "User name:"
                    inputText "name" v
                H.p $ do
                    label "password" v "Password:"
                    inputPassword "password" v
                inputSubmit "Log in"

requestPasswordResetPage :: View Html -> Html
requestPasswordResetPage v =
    H.docTypeHtml $ do
        H.head $ H.title "Reset your password"
        H.body $ do
            form v "reset_password_request" $ do
                H.p $ do
                    label "email" v "Email address: "
                    inputText "email" v
                inputSubmit "Reset your password"

requestPasswordResetForm :: Monad m => Form Html m UserEmail
requestPasswordResetForm =
    UserEmail <$> "email" .: check "email address must not be empty" nonEmpty (text Nothing)

resetPasswordPage :: Text -> View Html -> Html
resetPasswordPage reqUrl v =
    H.docTypeHtml $ do
        H.head $ H.title "Enter a new password"
        H.body $ do
            form v reqUrl $ do
                H.p $ do
                    label "password1" v "New password: "
                    inputPassword "password1" v
                H.p $ do
                    label "password2" v "repeat password: "
                    inputPassword "password2" v
                inputSubmit "Set your new password"

resetPasswordForm :: Monad m => Form Html m UserPass
resetPasswordForm = (validate validatePass) $
    (,)
      <$> (UserPass <$> "password1" .: check "password must not be empty" nonEmpty (text Nothing))
      <*> (UserPass <$> "password2" .: check "password must not be empty" nonEmpty (text Nothing))
  where
    validatePass :: (UserPass, UserPass) -> Result Html UserPass
    validatePass (p1, p2) = if p1 == p2
                                then Success p1
                                else Error "passwords don't match"

emailSentPage :: Html
emailSentPage = H.string $ "Please check your email"

errorPage :: String -> Html
errorPage errorString = H.string $ "Encountered error: " ++ show errorString

notLoggedInPage :: Html
notLoggedInPage = H.docTypeHtml $ do
    H.head $ H.title "Not logged in"
    H.body $ do
        H.p "You're currently not logged into Thentos."
        H.p $ "Please go to " <> loginLink <> " and try again."
  where
    loginLink = H.a ! A.href "/log_into_thentos" $ "login"


-- * auxillary functions

nonEmpty :: Text -> Bool
nonEmpty = not . T.null

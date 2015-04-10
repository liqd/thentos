{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Thentos.Frontend.Pages
    ( indexPage
    , userCreatePage
    , userCreateRequestedPage
    , userCreatedPage
    , serviceCreatePage
    , serviceCreateForm
    , serviceCreatedPage
    , userCreateForm
    , userUpdatePage
    , userUpdateForm
    , passwordUpdateForm
    , emailUpdateForm
    , loginServicePage
    , loginThentosPage
    , loginThentosForm
    , logoutThentosPage
    , resetPasswordRequestPage
    , resetPasswordRequestForm
    , resetPasswordPage
    , resetPasswordForm
    , resetPasswordRequestedPage
    , errorPage
    , notLoggedInPage
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.ByteString (ByteString)
import Data.Maybe (isJust, catMaybes)
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
import Thentos.DB.Trans (UpdateUserFieldOp(..))

indexPage :: Html
indexPage = do
    H.head $ do
        H.title "Thentos main page"
    H.body $ do
        H.h3 $ do
            "things you can do from here:"
        H.ul $ do
            H.li . (H.a ! A.href "/login_thentos") $ "login"
            H.li . (H.a ! A.href "/logout_thentos") $ "logout"
            H.li . (H.a ! A.href "/user/create") $ "create user"
            H.li . (H.a ! A.href "/service/create") $ "create service"
            H.li . (H.a ! A.href "/user/reset_password_request") $ "request password reset"

userCreatePage :: View Html -> Html
userCreatePage v = H.docTypeHtml $ do
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

userCreateForm :: Monad m => Form Html m UserFormData
userCreateForm = (validate validateUserData) $ (,,,)
    <$> (UserName  <$> "name"      .: check "name must not be empty"        nonEmpty   (text Nothing))
    <*> (UserPass <$> "password1"  .: check "password must not be empty"    nonEmpty   (text Nothing))
    <*> (UserPass <$> "password2"  .: check "password must not be empty"    nonEmpty   (text Nothing))
    <*> (UserEmail <$> "email"     .: check "must be a valid email address" checkEmail (text Nothing))
  where
    validateUserData (name, pw1, pw2, email)
        | pw1 == pw2 = Success $ UserFormData name pw1 email
        | otherwise  = Error "Passwords don't match"

userCreateRequestedPage :: Html
userCreateRequestedPage = H.string $ "Please check your email"

userCreatedPage :: UserId -> Html
userCreatedPage uid =
    H.docTypeHtml $ do
        H.head $
            H.title "Success!"
        H.body $ do
            H.h1 "Added a user!"
            H.pre . H.string $ show uid

userUpdatePage :: View Html -> View Html -> View Html -> Html
userUpdatePage userView emailView pwView = H.docTypeHtml $ do
    H.head $ H.title "Update user data"
    H.body $ do
        form userView "update" $ do
            H.p $ do
                label "name" userView "User name:"
                inputText "name" userView
            inputSubmit "Update User Data" ! A.id "update_user_submit"

        form pwView "update_password" $ do
            H.p $ do
                label "old_password" pwView "Current Password: "
                inputPassword "old_password" pwView
            H.p $ do
                label "new_password1" pwView "New password: "
                inputPassword "new_password1" pwView
            H.p $ do
                label "new_password2" pwView "Repeat new password: "
                inputPassword "new_password2" pwView
            inputSubmit "Update Password" ! A.id "update_password_submit"

        form emailView "update_email" $ do
            H.p $ do
                label "email" emailView "Email Address: "
                inputText "email" emailView
            inputSubmit "Update Email Address" ! A.id "update_email_submit"

-- this is a bit overkill for now, but easily extensible for new user data fields
userUpdateForm :: Monad m => Form Html m [UpdateUserFieldOp]
userUpdateForm = (validate validateUserData) $
    "name"      .: text Nothing
  where
    validateUserData :: Text -> Result Html [UpdateUserFieldOp]
    validateUserData name  =
        let updates = catMaybes [ validateName name
                                ]
        in if null updates
            then Error "Nothing to update"
            else Success updates

    validateName :: Text -> Maybe UpdateUserFieldOp
    validateName name = toMaybe (not $ T.null name)
                                (UpdateUserFieldName $ UserName name)

    toMaybe :: Bool -> a -> Maybe a
    toMaybe b v = if b then Just v else Nothing

passwordUpdateForm :: Monad m => Form Html m (UserPass, UserPass)
passwordUpdateForm = (validate newPasswordsMatch) $ (,,)
    <$> ("old_password"  .: check "password must not be empty" nonEmpty (text Nothing))
    <*> ("new_password1" .: check "password must not be empty" nonEmpty (text Nothing))
    <*> ("new_password2" .: check "password must not be empty" nonEmpty (text Nothing))
  where
    newPasswordsMatch (old, new1, new2)
        | new1 == new2 = Success (UserPass old, UserPass new1)
        | otherwise    = Error "passwords don't match"

emailUpdateForm :: Monad m => Form Html m UserEmail
emailUpdateForm =
    UserEmail <$> "email" .: check "must be a valid email address" checkEmail (text Nothing)

serviceCreatePage :: View Html -> Html
serviceCreatePage v = H.docTypeHtml $ do
    H.head $ do
        H.title "Create Service"
    H.body $ do
        form v "create" $ do
            H.p $ do
                label "name" v "Service name:"
                inputText "name" v
            H.p $ do
                label "description" v "Service description:"
                inputText "description" v
            inputSubmit "Create Service" ! A.id "create_service_submit"

serviceCreateForm :: Monad m => Form Html m (ServiceName, ServiceDescription)
serviceCreateForm =
    (,) <$>
        (ServiceName <$> "name" .: check "name must not be empty" nonEmpty (text Nothing)) <*>
        (ServiceDescription <$> "description" .: check "description must not be mpty" nonEmpty (text Nothing))

serviceCreatedPage :: ServiceId -> ServiceKey -> Html
serviceCreatedPage sid key = H.docTypeHtml $ do
    H.head $ do
        H.title "Service created!"
    H.body $ do
        H.body $ do
            H.h1 "Added a service!"
            H.p "Service id: " <> H.text (fromServiceId sid)
            H.p "Service key: " <> H.text (fromServiceKey key)

loginServicePage :: ServiceId -> View Html -> ByteString -> Html
loginServicePage (H.string . cs . fromServiceId -> serviceId) v reqURI =
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

loginThentosPage :: View Html -> Html
loginThentosPage v = do
    H.docTypeHtml $ do
        H.head $
            H.title "Log into thentos"
        H.body $ do
            form v "login_thentos" $ do
                H.p $ do
                    label "usernamme" v "User name:"
                    inputText "name" v
                H.p $ do
                    label "password" v "Password:"
                    inputPassword "password" v
                inputSubmit "Log in" ! A.id "login_submit"

loginThentosForm :: Monad m => Form Html m (UserName, UserPass)
loginThentosForm = (,)
    <$> (UserName  <$> "name"    .: check "name must not be empty"     nonEmpty   (text Nothing))
    <*> (UserPass <$> "password" .: check "password must not be empty" nonEmpty   (text Nothing))

logoutThentosPage :: Html
logoutThentosPage = do
    H.head $ H.title "Log out"
    H.body $ do
        H.form ! A.method "POST" ! A.action "logout_thentos" $
            H.input ! A.type_ "submit" ! A.value "Log Out" ! A.id "logout_submit"

resetPasswordRequestPage :: View Html -> Html
resetPasswordRequestPage v =
    H.docTypeHtml $ do
        H.head $ H.title "Reset your password"
        H.body $ do
            form v "reset_password_request" $ do
                H.p $ do
                    label "email" v "Email address: "
                    inputText "email" v
                inputSubmit "Reset your password"

resetPasswordRequestForm :: Monad m => Form Html m UserEmail
resetPasswordRequestForm =
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

resetPasswordRequestedPage :: Html
resetPasswordRequestedPage = H.string $ "Please check your email"

errorPage :: String -> Html
errorPage errorString = H.string $ "Encountered error: " ++ show errorString

notLoggedInPage :: Html
notLoggedInPage = H.docTypeHtml $ do
    H.head $ H.title "Not logged in"
    H.body $ do
        H.p "You're currently not logged into Thentos."
        H.p $ "Please go to " <> loginLink <> " and try again."
  where
    loginLink = H.a ! A.href "/login_thentos" $ "login"


-- * auxillary functions

nonEmpty :: Text -> Bool
nonEmpty = not . T.null

checkEmail :: Text -> Bool
checkEmail = isJust . T.find (== '@')

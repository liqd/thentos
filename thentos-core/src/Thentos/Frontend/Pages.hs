{-# LANGUAGE OverloadedStrings  #-}

-- | This module provides 'Html' pages and 'Form's (disgetive-functor thingies that parse filled-out
-- forms).  The pages come in three flavours:
--
-- - *Pages* (starting with html tag and without holes);
-- - *Pagelets* (functions that return *complete pages*, but contain holes that need to be filled
--    with the function's 'Html' argument(s));
-- - *Snippets*: 'Html' elements for filling holes in pagelets or other snippets.
module Thentos.Frontend.Pages
    ( dashboardPagelet

    , userRegisterPage
    , userRegisterForm
    , userRegisterRequestedPage

    , userLoginPage
    , userLoginForm

    , resetPasswordRequestPage
    , resetPasswordRequestForm
    , resetPasswordRequestedPage
    , resetPasswordPage
    , resetPasswordForm

    , userLogoutConfirmSnippet
    , userLogoutDonePage

    , userDisplaySnippet
    , userServicesDisplaySnippet
    , emailUpdateSnippet
    , emailUpdateForm
    , passwordUpdateSnippet
    , passwordUpdateForm

    , serviceCreateSnippet
    , serviceCreateForm
    , serviceRegisterPage
    , serviceRegisterForm

    , errorPage
    , errorPagelet
    , permissionDeniedPage
    , notFoundPage
    , confirmationMailSentPage
    , confirmationMailSentSnippet
    ) where

import Control.Lens ((^.))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (ST)
import Data.String (IsString)
import Text.Blaze.Html (Html, (!), ToValue(toValue))
import Text.Digestive.Blaze.Html5 (form, inputText, inputPassword, label, inputSubmit, childErrorList)
import Text.Digestive.Form (Form, check, validate, text, (.:))
import Text.Digestive.Types (Result(Success, Error))
import Text.Digestive.View (View)

import qualified Data.Text as ST
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Thentos.Frontend.Types
import Thentos.Types


-- * base

-- | Call 'basePagelet'' without optional headings.
basePagelet :: ST -> Html -> Html
basePagelet title = basePagelet' title Nothing

-- | Create an html document with default headings from title,
-- optional headings, and body.
basePagelet' :: ST -> Maybe Html -> Html -> Html
basePagelet' title mHeadings body = H.docTypeHtml $ do
    H.head $ do
        H.title $ H.text title
        H.link H.! A.rel "stylesheet" H.! A.href "/screen.css"
        fromMaybe (return ()) mHeadings
    H.body $ do
        H.h1 $ H.text title
        body

-- | Protect a form from CSRF attacks by including a secret token as a hidden
-- field.
csrfProofForm :: ST -> View Html -> ST -> Html -> Html
csrfProofForm csrfToken v action html = form v action (html <> makeCsrfField csrfToken)

makeCsrfField :: ST -> Html
makeCsrfField csrfToken =
    H.input H.! A.type_ "hidden" H.! A.name "_csrf" H.! A.value (toValue csrfToken)


-- * dashboard

-- | The dashboard is the frame of what the user always sees when
-- logged in.  The dashboard body shows further specifics.  It is the
-- caller's responsibility to make sure that dashboard state and body
-- correspond.
dashboardPagelet :: [FrontendMsg] -> [Role] -> DashboardTab -> Html -> Html
dashboardPagelet msgs availableRoles ((==) -> isActive) body =
    basePagelet "Thentos Dashboard" $ do
        H.div . H.ul . mapM_ (H.li . H.string . show) $ msgs
        H.div . H.table . H.tr $ mapM_ tabLink [minBound..]
        H.div H.! A.class_ "dashboard_body" $ body
  where
    tabLink :: DashboardTab -> Html
    tabLink tab
        | not available = return ()
        | otherwise     = H.td $ H.div ! A.class_ className $ H.a ! A.href urlt $ linkt
      where
        available :: Bool
        available = all (`elem` availableRoles) (needsRoles tab)

        className :: H.AttributeValue
        className = if isActive tab then "active_tab_header" else "inactive_tab_header"

        linkt :: Html
        linkt = H.text . linkText $ tab

        urlt :: H.AttributeValue
        urlt = H.textValue $ linkUrl tab

    needsRoles :: DashboardTab -> [Role]
    needsRoles DashboardTabDetails = []
    needsRoles DashboardTabServices = [RoleUser]
    needsRoles DashboardTabOwnServices = [RoleServiceAdmin]
    needsRoles DashboardTabUsers = [RoleUserAdmin]
    needsRoles DashboardTabLogout = []

    linkText :: DashboardTab -> ST
    linkText DashboardTabDetails     = "details"
    linkText DashboardTabServices    = "services"
    linkText DashboardTabOwnServices = "own services"
    linkText DashboardTabUsers       = "users"
    linkText DashboardTabLogout      = "logout"

    linkUrl  :: DashboardTab -> ST
    linkUrl DashboardTabDetails     = "/dashboard/details"       -- FIXME: not implemented
    linkUrl DashboardTabServices    = "/dashboard/services"      -- FIXME: not implemented
    linkUrl DashboardTabOwnServices = "/dashboard/ownservices"   -- FIXME: not implemented
    linkUrl DashboardTabUsers       = "/dashboard/users"         -- FIXME: not implemented
    linkUrl DashboardTabLogout      = "/user/logout"

data DashboardTab =
    DashboardTabDetails
  | DashboardTabServices
  | DashboardTabOwnServices
  | DashboardTabUsers
  | DashboardTabLogout
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable)


-- * register (thentos)

userRegisterPage :: ST -> ST -> View Html -> Html
userRegisterPage csrfToken formAction v = basePagelet "Create User" $ do
    childErrorList "" v
    csrfProofForm csrfToken v formAction $ do
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

userRegisterForm :: Monad m => Form Html m UserFormData
userRegisterForm = validate validateUserData $ (,,,)
    <$> (UserName <$> "name"      .: validateNonEmpty "name"     (text Nothing))
    <*> (UserPass <$> "password1" .: validateNonEmpty "password" (text Nothing))
    <*> (UserPass <$> "password2" .: validateNonEmpty "password" (text Nothing))
    <*> (             "email"     .: validateEmail               (text Nothing))
  where
    validateUserData (name, pw1, pw2, email)
        | pw1 == pw2           = Success $ UserFormData name pw1 email
        | otherwise            = Error "Passwords don't match"


userRegisterRequestedPage :: Html
userRegisterRequestedPage = confirmationMailSentPage "Create User"
    "Thank you for your registration." "your registration"


-- * login (thentos)

userLoginPage :: Maybe ST -> ST -> ST -> View Html -> Html
userLoginPage mMsg csrfToken formAction v = basePagelet "Thentos Login" $ do
    childErrorList "" v
    csrfProofForm csrfToken v formAction $ do
        case mMsg of
            Just msg -> H.p $ H.text msg
            Nothing  -> return ()
        H.table $ do
            H.tr $ do
                H.td $ label "usernamme" v "Username"
                H.td $ inputText "name" v
            H.tr $ do
                H.td $ label "password" v "Password"
                H.td $ inputPassword "password" v
            H.tr $ do
                H.td $ pure ()
                H.td $ inputSubmit "Log in" ! A.id "login_submit"
            H.tr $ do
                H.td $ pure ()
                H.td $ H.a ! A.href "/user/register" ! A.id "login_create_new" $ "Register new user"
            H.tr $ do
                H.td $ pure ()
                H.td $ H.a ! A.href "/user/reset_password_request" ! A.id "login_forgot_password" $ "forgot password?"

userLoginForm :: Monad m => Form Html m (UserName, UserPass)
userLoginForm = (,)
    <$> (UserName <$> "name"     .: validateNonEmpty "name"     (text Nothing))
    <*> (UserPass <$> "password" .: validateNonEmpty "password" (text Nothing))


-- * forgot password

resetPasswordPage :: ST -> ST -> View Html -> Html
resetPasswordPage csrfToken formAction v = basePagelet "Thentos Login" $ do
    childErrorList "" v
    csrfProofForm csrfToken v formAction $ do
        H.p $ do
            H.text "You can send yourself an email with a link to the password reset page."
        H.p $ do
            label "email" v "Email address: "
            inputText "email" v
        inputSubmit "Send"

resetPasswordForm :: Monad m => Form Html m UserEmail
resetPasswordForm = "email" .: validateEmail (text Nothing)

resetPasswordConfirmPage :: ST -> ST -> View Html -> Html
resetPasswordConfirmPage csrfToken formAction v = basePagelet "Thentos Login" $ do
    childErrorList "" v
    csrfProofForm csrfToken v formAction $ do
        H.p $ do
            label "password1" v "New password: "
            inputPassword "password1" v
        H.p $ do
            label "password2" v "Repeat password: "
            inputPassword "password2" v
        inputSubmit "Set your new password"

resetPasswordConfirmForm :: Monad m => Form Html m UserPass
resetPasswordConfirmForm = validate validatePass $ (,)
    <$> (UserPass <$> "password1" .: validateNonEmpty "password" (text Nothing))
    <*> (UserPass <$> "password2" .: validateNonEmpty "password" (text Nothing))

resetPasswordRequestedPage :: Html
resetPasswordRequestedPage = confirmationMailSentPage "Password Reset"
    "Thank you for your password reset request." "the process"


-- * logout (thentos)

userLogoutConfirmPagelet :: ST -> [ServiceName] -> ST -> u -> rs -> Html
userLogoutConfirmPagelet formAction serviceNames csrfToken _ _ = do
    H.p . H.text . ST.unlines $
        "You are about to logout from thentos." :
        "This will log you out from the following services/sites:" :
        []
    case serviceNames of
        []    -> H.p "(none)"
        (_:_) -> H.ul $ mapM_ (H.li . H.text . fromServiceName) serviceNames
    H.table . H.tr $ do
        H.td $ do
            H.form ! A.method "POST" ! A.action (H.textValue formAction) $ do
                H.input ! A.type_ "submit" ! A.value "Log Out" ! A.id "logout_submit"
                makeCsrfField csrfToken
        H.td $ do
            H.a ! A.href "/dashboard" $ "Back to dashboard"

userLogoutDonePage :: Html
userLogoutDonePage = basePagelet "Thentos Logout" $ do
    H.p "You have been logged out of Thentos."
    H.p $ H.a ! A.href "/user/login" $ "Log back in"


-- * update user

userDisplayPagelet :: User -> rs -> Html
userDisplayPagelet user _ = do
    H.table $ do
        H.tr $ do
            H.td . H.text $ "name"
            H.td . H.text $ fromUserName (user ^. userName)
        H.tr $ do
            H.td . H.text $ "email"
            H.td . H.text $ fromUserEmail (user ^. userEmail)
        H.tr $ do
            H.td . H.text $ "street"
            H.td . H.text $ "n/a"
        H.tr $ do
            H.td . H.text $ "postal code"
            H.td . H.text $ "n/a"
        H.tr $ do
            H.td . H.text $ "city"
            H.td . H.text $ "n/a"
        H.tr $ do
            H.td . H.text $ "country"
            H.td . H.text $ "n/a"
        H.tr $ do
            H.td $ pure ()
            H.td $ H.a ! A.href "/user/update_password" $ "change password"
        H.tr $ do
            H.td $ pure ()
            H.td $ H.a ! A.href "/user/update_email" $ "change email"
        H.tr $ do
            H.td $ pure ()
            H.td $ H.a ! A.href "/n/a" $ "delete"


-- | (this is just a dummy.)
userServicesDisplayPagelet :: u -> rs -> Html
userServicesDisplayPagelet _ _ = do
    H.table $ do
        H.tr $ do
            H.td . H.ol $ mapM_ H.li ["Evil Corp.", "Facebook", H.b "Faceboot", "mein.berlin.de"]
            H.td . H.table $ do
                H.tr $ H.td "Service ID" >> H.td "Faceboot"
                H.tr $ H.td "Description" >> H.td "Something about boats?"
                H.tr $ H.td "Logged in: " >> H.td "Yes"
                H.tr $ H.td "Session expires: " >> H.td "in a month"


userUpdatePagelet :: ST -> ST -> View Html -> u -> rs -> Html
userUpdatePagelet csrfToken formAction v _ _ = do
    childErrorList "" v
    csrfProofForm csrfToken v formAction $ do
        H.p $ do
            label "name" v "User name: "
            inputText "name" v
        inputSubmit "Update User Data" ! A.id "update_user_submit"


passwordUpdatePagelet :: ST -> ST -> View Html -> u -> rs -> Html
passwordUpdatePagelet csrfToken formAction v _ _ = do
    childErrorList "" v
    csrfProofForm csrfToken v formAction $ do
        H.p $ do
            label "old_password" v "Current Password: "
            inputPassword "old_password" v
        H.p $ do
            label "new_password1" v "New password: "
            inputPassword "new_password1" v
        H.p $ do
            label "new_password2" v "Repeat new password: "
            inputPassword "new_password2" v
        inputSubmit "Update Password" ! A.id "update_password_submit"

passwordUpdateForm :: Monad m => Form Html m (UserPass, UserPass)
passwordUpdateForm = validate validatePassChange $ (,,)
    <$> (UserPass <$> "old_password"  .:                              text Nothing)
    <*> (UserPass <$> "new_password1" .: validateNonEmpty "password" (text Nothing))
    <*> (UserPass <$> "new_password2" .: validateNonEmpty "password" (text Nothing))


emailUpdatePagelet :: ST -> ST -> View Html -> u -> rs -> Html
emailUpdatePagelet csrfToken formAction v _ _ = do
    childErrorList "" v
    csrfProofForm csrfToken v formAction $ do
        H.p $ do
            label "email" v "Email Address: "
            inputText "email" v
        inputSubmit "Update Email Address" ! A.id "update_email_submit"

emailUpdateForm :: Monad m => Form Html m UserEmail
emailUpdateForm = "email" .: validateEmail (text Nothing)


-- * services

serviceCreatePagelet :: ST -> ST -> View Html -> u -> rs -> Html
serviceCreatePagelet csrfToken formAction v _ _ = basePagelet "Create Service" $ do
    childErrorList "" v
    csrfProofForm csrfToken v formAction $ do
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
        (ServiceName        <$> "name"        .: validateNonEmpty "name" (text Nothing)) <*>
        (ServiceDescription <$> "description" .:                          text Nothing)

-- (this is an empty form for now, but in the future, the user will
-- want to decide what data to pass on to the service here.)
serviceRegisterPage :: ST -> ST -> View Html -> ServiceId -> Service -> User -> Html
serviceRegisterPage csrfToken formAction v sid service user = basePagelet "Register with Service" $ do
    childErrorList "" v
    csrfProofForm csrfToken v formAction $ do
        H.hr
        H.p $ "Your name: " <> H.text (fromUserName $ user ^. userName)
        H.p $ "Your email: " <> H.text (fromUserEmail $ user ^. userEmail)
        H.hr
        H.p $ "Service id: " <> H.text (fromServiceId sid)
        H.p $ "Service name: " <> H.text (fromServiceName $ service ^. serviceName)
        H.p $ "Service description: " <> H.text (fromServiceDescription $ service ^. serviceDescription)
        H.hr
        inputSubmit "Register!"

serviceRegisterForm :: Monad m => Form Html m ()
serviceRegisterForm = pure ()


-- * util

-- ** error / status reports to the user

errorPage :: String -> Html
errorPage = basePagelet "Error" . errorHtml

errorPagelet :: u -> rs -> String -> Html
errorPagelet _ _ = errorHtml

errorHtml :: String -> Html
errorHtml = H.string . ("*** error: " ++) . show

permissionDeniedPage :: Html
permissionDeniedPage = basePagelet' "Permission Denied"
                                    Nothing
                                    (H.a ! A.href "/dashboard" $ "Back to dashboard")

notFoundPage :: Html
notFoundPage = basePagelet "Not Found" $ H.p "The requested page does not exist."

confirmationMailSentPage :: ST -> ST -> ST -> Html
confirmationMailSentPage title msg1 msg2 = basePagelet title $ confirmationMailSentBody msg1 msg2

confirmationMailSentPagelet :: ST -> ST -> u -> rs -> Html
confirmationMailSentPagelet msg1 msg2 _ _ = confirmationMailSentBody msg1 msg2

confirmationMailSentBody :: ST -> ST -> Html
confirmationMailSentBody msg1 msg2 = H.p . H.text . ST.unlines $
    msg1 :
    "Please check your email (don't forget the spam folder)" :
    "and complete " <> msg2 <> " by following the link we sent you." :
    []


-- ** form field tests

validateNonEmpty :: (Monoid v, IsString v, Monad m) => v -> Form v m ST -> Form v m ST
validateNonEmpty fieldName = check (fieldName <> " must not be empty") (not . ST.null)

validateEmail :: (Monoid v, IsString v, Monad m) => Form v m ST -> Form v m UserEmail
validateEmail = validate $ maybe (Error "email address invalid") Success . parseUserEmail

validatePass :: (UserPass, UserPass) -> Result Html UserPass
validatePass (p1, p2) = if p1 == p2
    then Success p1
    else Error "passwords don't match"

validatePassChange :: (UserPass, UserPass, UserPass) -> Result Html (UserPass, UserPass)
validatePassChange (old, new1, new2) = if new1 == new2
    then Success (old, new1)
    else Error "passwords don't match"

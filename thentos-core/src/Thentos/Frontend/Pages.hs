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
import Control.Monad (when)
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
basePagelet :: FrontendSessionData -> ST -> Html -> Html
basePagelet fsd title = basePagelet' fsd title Nothing

-- | Create an html document with default headings from title,
-- optional headings, and body.
basePagelet' :: FrontendSessionData -> ST -> Maybe Html -> Html -> Html
basePagelet' fsd title mHeadings body = H.docTypeHtml $ do
    H.head $ do
        H.title $ H.text title
        H.link H.! A.rel "stylesheet" H.! A.href "/screen.css"
        sequence_ mHeadings
    H.body $ do
        H.div . H.ul . mapM_ (H.li . H.string . show) $ (fsd ^. fsdMessages)
        H.h1 $ H.text title
        body

-- | Protect a form from CSRF attacks by including a secret token as a hidden field.
csrfProofForm :: FrontendSessionData -> View Html -> ST -> Html -> Html
csrfProofForm _ v action = form v action . (<> csrfField)
  where
    csrfToken :: ST
    csrfToken = "wef"  -- BUG #400: get from state

    csrfField :: Html
    csrfField = H.input H.! A.type_ "hidden" H.! A.name "_csrf" H.! A.value (toValue csrfToken)


-- * dashboard

-- | The dashboard is the frame of what the user always sees when
-- logged in.  The dashboard body shows further specifics.  It is the
-- caller's responsibility to make sure that dashboard state and body
-- correspond.
dashboardPagelet :: FrontendSessionData -> [Group] -> Html -> Html
dashboardPagelet fsd availableRoles body =
    basePagelet fsd "Thentos Dashboard" $ do
        H.div . H.table . H.tr $ mapM_ tabLink [minBound..]
        H.div H.! A.class_ "dashboard_body" $ body
  where
    tabLink :: DashboardTab -> Html
    tabLink tab =
        when available $
            H.td $ H.div ! A.class_ className $ H.a ! A.href urlt $ linkt
      where
        available :: Bool
        available = all (`elem` availableRoles) (needsRoles tab)

        className :: H.AttributeValue
        className = if ((fsd ^. fsdLogin) >>= (^. fslDashboardTab)) == Just tab
            then "active_tab_header"
            else "inactive_tab_header"

        linkt :: Html
        linkt = H.text . linkText $ tab

        urlt :: H.AttributeValue
        urlt = H.textValue $ linkUrl tab

    needsRoles :: DashboardTab -> [Group]
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
    linkUrl DashboardTabDetails     = "/dashboard/details"
    linkUrl DashboardTabServices    = "/dashboard/services"
    linkUrl DashboardTabOwnServices = "/dashboard/ownservices"
    linkUrl DashboardTabUsers       = "/dashboard/users"
    linkUrl DashboardTabLogout      = "/user/logout"


-- * register (thentos)

userRegisterPage :: FrontendSessionData -> View Html -> ST -> Html
userRegisterPage fsd v formAction = basePagelet fsd "Create User" $ do
    childErrorList "" v
    csrfProofForm fsd v formAction $ do
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


userRegisterRequestedPage :: FrontendSessionData -> Html
userRegisterRequestedPage fsd = confirmationMailSentPage fsd "Create User"
    "Thank you for your registration." "your registration"


-- * login (thentos)

userLoginPage :: FrontendSessionData -> View Html -> ST -> Html
userLoginPage fsd v formAction = basePagelet fsd "Thentos Login" $ do
    childErrorList "" v
    csrfProofForm fsd v formAction $ do
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

resetPasswordRequestPage :: FrontendSessionData -> View Html -> ST -> Html
resetPasswordRequestPage fsd v formAction = basePagelet fsd "Thentos Login" $ do
    childErrorList "" v
    csrfProofForm fsd v formAction $ do
        H.p $ do
            H.text "You can send yourself an email with a link to the password reset page."
        H.p $ do
            label "email" v "Email address: "
            inputText "email" v
        inputSubmit "Send"

resetPasswordRequestForm :: Monad m => Form Html m UserEmail
resetPasswordRequestForm = "email" .: validateEmail (text Nothing)

resetPasswordRequestedPage :: FrontendSessionData -> Html
resetPasswordRequestedPage fsd = confirmationMailSentPage fsd "Password Reset"
    "Thank you for your password reset request." "the process"

resetPasswordPage :: FrontendSessionData -> View Html -> ST -> Html
resetPasswordPage fsd v formAction = basePagelet fsd "Thentos Login" $ do
    childErrorList "" v
    csrfProofForm fsd v formAction $ do
        H.p $ do
            label "password1" v "New password: "
            inputPassword "password1" v
        H.p $ do
            label "password2" v "Repeat password: "
            inputPassword "password2" v
        inputSubmit "Set your new password"

resetPasswordForm :: Monad m => Form Html m UserPass
resetPasswordForm = validate validatePass $ (,)
    <$> (UserPass <$> "password1" .: validateNonEmpty "password" (text Nothing))
    <*> (UserPass <$> "password2" .: validateNonEmpty "password" (text Nothing))


-- * logout (thentos)

userLogoutConfirmSnippet :: ST -> [ServiceName] -> ST -> u -> rs -> Html
userLogoutConfirmSnippet formAction serviceNames _ _ _ = do
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
                -- makeCsrfField csrfToken
                -- BUG #401: do we need csrf protection here?  if so: did this ever work?
        H.td $ do
            H.a ! A.href "/dashboard" $ "Back to dashboard"

userLogoutDonePage :: FrontendSessionData -> Html
userLogoutDonePage fsd = basePagelet fsd "Thentos Logout" $ do
    H.p "You have been logged out of Thentos."
    H.p $ H.a ! A.href "/user/login" $ "Log back in"


-- * update user

userDisplaySnippet :: User -> rs -> Html
userDisplaySnippet user _ = do
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
userServicesDisplaySnippet :: u -> rs -> Html
userServicesDisplaySnippet _ _ = do
    H.table $ do
        H.tr $ do
            H.td . H.ol $ mapM_ H.li ["Evil Corp.", "Facebook", H.b "Faceboot", "mein.berlin.de"]
            H.td . H.table $ do
                H.tr $ H.td "Service ID" >> H.td "Faceboot"
                H.tr $ H.td "Description" >> H.td "Something about boats?"
                H.tr $ H.td "Logged in: " >> H.td "Yes"
                H.tr $ H.td "Session expires: " >> H.td "in a month"


emailUpdateSnippet :: FrontendSessionData -> View Html -> ST -> u -> rs -> Html
emailUpdateSnippet fsd v formAction _ _ = do
    childErrorList "" v
    csrfProofForm fsd v formAction $ do
        H.p $ do
            label "email" v "Email Address: "
            inputText "email" v
        inputSubmit "Update Email Address" ! A.id "update_email_submit"

emailUpdateForm :: Monad m => Form Html m UserEmail
emailUpdateForm = "email" .: validateEmail (text Nothing)


passwordUpdateSnippet :: FrontendSessionData -> View Html -> ST -> u -> rs -> Html
passwordUpdateSnippet fsd v formAction _ _ = do
    childErrorList "" v
    csrfProofForm fsd v formAction $ do
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


-- * services

serviceCreateSnippet :: FrontendSessionData -> View Html -> ST -> u -> rs -> Html
serviceCreateSnippet fsd v formAction _ _ = do
    childErrorList "" v
    csrfProofForm fsd v formAction $ do
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

serviceRegisterPage :: FrontendSessionData -> View Html -> ST -> ServiceId -> Service -> User -> Html
serviceRegisterPage fsd v formAction sid service user = basePagelet fsd "Register with Service" $ do
    childErrorList "" v
    csrfProofForm fsd v formAction $ do
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
errorPage = basePagelet fsd "Error" . errorHtml
  where
    fsd = emptyFrontendSessionData

errorPagelet :: u -> rs -> String -> Html
errorPagelet _ _ = errorHtml

errorHtml :: String -> Html
errorHtml = H.string . ("*** error: " ++) . show

permissionDeniedPage :: Html
permissionDeniedPage = basePagelet' fsd "Permission Denied" Nothing
    (H.a ! A.href "/dashboard" $ "Back to dashboard")
  where
    fsd = emptyFrontendSessionData

notFoundPage :: Html
notFoundPage = basePagelet fsd "Not Found" $ H.p "The requested page does not exist."
  where
    fsd = emptyFrontendSessionData

confirmationMailSentPage :: FrontendSessionData -> ST -> ST -> ST -> Html
confirmationMailSentPage fsd title msg1 msg2 =
    basePagelet fsd title $ confirmationMailSentBody msg1 msg2

confirmationMailSentSnippet :: ST -> ST -> u -> rs -> Html
confirmationMailSentSnippet msg1 msg2 _ _ = confirmationMailSentBody msg1 msg2

confirmationMailSentBody :: ST -> ST -> Html
confirmationMailSentBody msg1 msg2 = H.p . H.text . ST.unlines $
    msg1 :
    "Please check your email (don't forget the spam folder)" :
    "and complete " <> msg2 <> " by following the link we sent you." :
    []


-- ** form field validation

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

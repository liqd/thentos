{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}

module Thentos.Frontend.Pages
    ( dashboardPagelet
    , DashboardTab(..)

    , userRegisterPage
    , userRegisterForm
    , userRegisterRequestedPage

    , userLoginPage
    , userLoginForm

    , resetPasswordPagelet
    , resetPasswordForm
    , resetPasswordRequestPagelet
    , resetPasswordRequestForm
    , resetPasswordRequestedPagelet

    , userLogoutConfirmPagelet
    , userLogoutDonePage

    , userDisplayPagelet
    , userUpdatePagelet
    , userUpdateForm
    , emailUpdatePagelet
    , emailUpdateForm
    , passwordUpdatePagelet
    , passwordUpdateForm

    , serviceCreatePage
    , serviceCreateForm
    , serviceCreatedPage
    , serviceRegisterPage
    , serviceRegisterForm
    , serviceLoginPage

    , errorPage
    , errorPagelet
    , confirmationMailSentPage
    , confirmationMailSentPagelet
    ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Lens ((^.))
import Data.Maybe (isJust, catMaybes, fromMaybe)
import Data.Monoid (Monoid, (<>))
import Data.String.Conversions (ST, cs)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Text.Blaze.Html (Html, (!))
import Text.Digestive.Blaze.Html5 (form, inputText, inputPassword, label, inputSubmit, childErrorList)
import Text.Digestive.Form (Form, check, validate, text, (.:))
import Text.Digestive.Types (Result(Success, Error))
import Text.Digestive.View (View)

import qualified Data.Text as ST
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Thentos.Types
import Thentos.DB.Trans (UpdateUserFieldOp(..))


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


-- * dashboard

-- | The dashboard is the frame of what the user always sees when
-- logged in.  The dashboard body shows further specifics.  It is the
-- caller's responsibility to make sure that dashboard state and body
-- correspond.
dashboardPagelet :: [Role] -> DashboardTab -> Html -> Html
dashboardPagelet availableRoles ((==) -> isActive) body =
    basePagelet "Thentos Dashboard" $ do
        H.div . H.table . H.tr $ mapM_ tabLink [minBound..]
        H.div H.! A.class_ "dashboard_body" $ body
  where
    tabLink :: DashboardTab -> Html
    tabLink tab
        | not available = return ()
        | isActive tab  = H.td $ H.b linkt
        | True          = H.td $ H.a ! A.href urlt $ linkt
      where
        available :: Bool
        available = all (`elem` availableRoles) (needsRoles tab)

        linkt :: Html
        linkt = H.text . linkText $ tab

        urlt :: H.AttributeValue
        urlt = H.textValue $ linkUrl tab

    needsRoles :: DashboardTab -> [Role]
    needsRoles DashboardTabDetails = []
    needsRoles DashboardTabServices = [RoleBasic RoleUser]
    needsRoles DashboardTabOwnServices = [RoleBasic RoleServiceAdmin]
    needsRoles DashboardTabUsers = [RoleBasic RoleUserAdmin]
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

userRegisterPage :: ST -> View Html -> Html
userRegisterPage formAction v =  basePagelet "Create User" $ do
    childErrorList "" v
    form v formAction $ do
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
    <$> (UserName  <$> "name"      .: validateNonEmpty "name"     (text Nothing))
    <*> (UserPass  <$> "password1" .: validateNonEmpty "password" (text Nothing))
    <*> (UserPass  <$> "password2" .: validateNonEmpty "password" (text Nothing))
    <*> (UserEmail <$> "email"     .: validateEmail               (text Nothing))
  where
    validateUserData (name, pw1, pw2, email)
        | pw1 == pw2 = Success $ UserFormData name pw1 email
        | otherwise  = Error "Passwords don't match"

userRegisterRequestedPage :: Html
userRegisterRequestedPage = confirmationMailSentPage "Create User"
    "Thank you for your registration." "your registration"


-- * login (thentos)

userLoginPage :: Maybe ST -> ST -> View Html -> Html
userLoginPage mMsg formAction v = basePagelet "Thentos Login" $ do
    childErrorList "" v
    form v formAction $ do
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
                H.td $ H.a ! A.href "/user/reset_password" ! A.id "login_forgot_password" $ "forgot password?"

userLoginForm :: Monad m => Form Html m (UserName, UserPass)
userLoginForm = (,)
    <$> (UserName <$> "name"     .: validateNonEmpty "name"     (text Nothing))
    <*> (UserPass <$> "password" .: validateNonEmpty "password" (text Nothing))


-- * forgot password

resetPasswordRequestPagelet :: ST -> View Html -> u -> rs -> Html
resetPasswordRequestPagelet formAction v _ _ = do
    childErrorList "" v
    form v formAction $ do
        H.p $ do
            H.text "You can send yourself an email with a link to the password reset page."
        H.p $ do
            label "email" v "Email address: "
            inputText "email" v
        inputSubmit "Send"

resetPasswordRequestForm :: Monad m => Form Html m UserEmail
resetPasswordRequestForm =
    UserEmail <$> "email" .: validateEmail (text Nothing)

resetPasswordPagelet :: ST -> View Html -> u -> rs -> Html
resetPasswordPagelet formAction v _ _ = do
    childErrorList "" v
    form v formAction $ do
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

resetPasswordRequestedPagelet :: u -> rs -> Html
resetPasswordRequestedPagelet = confirmationMailSentPagelet
    "Thank you for your password reset request." "the process"


-- * logout (thentos)

userLogoutConfirmPagelet :: ST -> [ServiceName] -> u -> rs -> Html
userLogoutConfirmPagelet formAction serviceNames _ _ = do
    H.p . H.text . ST.unlines $
        "You are about to logout from thentos." :
        "This will log you out from the following services/sites:" :
        []
    case serviceNames of
        []    -> H.p "(none)"
        (_:_) -> H.ul $ mapM_ (H.li . H.text . fromServiceName) serviceNames
    H.table . H.tr $ do
        H.td $ do
            H.form ! A.method "POST" ! A.action (H.textValue formAction) $
                H.input ! A.type_ "submit" ! A.value "Log Out" ! A.id "logout_submit"
        H.td $ do
            H.a ! A.href "/dashboard" $ "Back to dashboard"

userLogoutDonePage :: Html
userLogoutDonePage = basePagelet "Thentos Logout" $
    H.p "You have been logged out of Thentos."


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
            H.td $ H.a ! A.href "/user/update" $ "edit"
        H.tr $ do
            H.td $ pure ()
            H.td $ H.a ! A.href "/user/update_password" $ "change password"
        H.tr $ do
            H.td $ pure ()
            H.td $ H.a ! A.href "/user/update_email" $ "change email"
        H.tr $ do
            H.td $ pure ()
            H.td $ H.a ! A.href "/n/a" $ "delete"


userUpdatePagelet :: ST -> View Html -> u -> rs -> Html
userUpdatePagelet formAction v _ _ = do
    childErrorList "" v
    form v formAction $ do
        H.p $ do
            label "name" v "User name: "
            inputText "name" v
        inputSubmit "Update User Data" ! A.id "update_user_submit"

-- | This is a bit overkill for now, but easily extensible for new user data fields.
userUpdateForm :: Monad m => UserName -> Form Html m [UpdateUserFieldOp]
userUpdateForm uname =
    validate validateUserData $
        "name" .: text (Just (fromUserName uname))
  where
    validateUserData :: ST -> Result Html [UpdateUserFieldOp]
    validateUserData name  =
        let updates = catMaybes [ validateName name
                                ]
        in if null updates
            then Error "Nothing to update"
            else Success updates

    validateName :: ST -> Maybe UpdateUserFieldOp
    validateName name = toMaybe (not $ ST.null name)
                                (UpdateUserFieldName $ UserName name)

    toMaybe :: Bool -> a -> Maybe a
    toMaybe True  v = Just v
    toMaybe False _ = Nothing

passwordUpdatePagelet :: ST -> View Html -> u -> rs -> Html
passwordUpdatePagelet formAction v _ _ = do
    childErrorList "" v
    form v formAction $ do
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


emailUpdatePagelet :: ST -> View Html -> u -> rs -> Html
emailUpdatePagelet formAction v _ _ = do
    childErrorList "" v
    form v formAction $ do
        H.p $ do
            label "email" v "Email Address: "
            inputText "email" v
        inputSubmit "Update Email Address" ! A.id "update_email_submit"

emailUpdateForm :: Monad m => Form Html m UserEmail
emailUpdateForm =
    UserEmail <$> "email" .: validateEmail (text Nothing)


-- * services

serviceCreatePage :: ST -> View Html -> Html
serviceCreatePage formAction v = basePagelet "Create Service" $ do
    childErrorList "" v
    form v formAction $ do
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

serviceCreatedPage :: ServiceId -> ServiceKey -> Html
serviceCreatedPage sid key = basePagelet "Create Service" $ do
    H.h1 "Added a service!"
    H.p $ "Service id: " <> H.text (fromServiceId sid)
    H.p $ "Service key: " <> H.text (fromServiceKey key)

-- (this is an empty form for now, but in the future, the user will
-- want to decide what data to pass on to the service here.)
serviceRegisterPage :: ST -> View Html -> ServiceId -> Service -> User -> Html
serviceRegisterPage formAction v sid service user = basePagelet "Register with Service" $ do
    childErrorList "" v
    form v formAction $ do
        H.h1 "You are about to register to a service"
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

serviceLoginPage :: ST -> ServiceId -> View Html -> Html
serviceLoginPage formAction (H.string . cs . fromServiceId -> serviceId) v = basePagelet "Log In (Service)" $ do
    H.p $ do
        "service id: " <> serviceId
    childErrorList "" v
    form v formAction $ do
        H.p $ do
            label "usernamme" v "User name:"
            inputText "name" v
        H.p $ do
            label "password" v "Password:"
            inputPassword "password" v
        inputSubmit "Log In"


-- * util

-- ** error / status reports to the user

errorPage :: String -> Html
errorPage errorString = basePagelet "Error" . H.string $ "*** error: " ++ show errorString

errorPagelet :: u -> rs -> String -> Html
errorPagelet _ _ errorString = H.string $ "*** error: " ++ show errorString

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

validateEmail :: (Monoid v, IsString v, Monad m) => Form v m ST -> Form v m ST
validateEmail = check "must be a valid email address" (isJust . ST.find (== '@'))

validatePass :: (UserPass, UserPass) -> Result Html UserPass
validatePass (p1, p2) = if p1 == p2
    then Success p1
    else Error "passwords don't match"

validatePassChange :: (UserPass, UserPass, UserPass) -> Result Html (UserPass, UserPass)
validatePassChange (old, new1, new2) = if new1 == new2
    then Success (old, new1)
    else Error "passwords don't match"

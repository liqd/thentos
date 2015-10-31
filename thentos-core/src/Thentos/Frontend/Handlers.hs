{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Thentos.Frontend.Handlers where

import Control.Lens
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader
import Control.Monad.State (gets, modify)
import Control.Monad.Trans.Class (lift)
import Data.Configifier (Tagged(Tagged), (>>.))
import Data.Monoid
import Data.String.Conversions
import Data.Void
import GHC.TypeLits (Symbol)
import Servant
import Servant.HTML.Blaze
import Servant.Missing
import Text.Blaze.Html
import Text.Digestive.Blaze.Html5
import Text.Digestive.View
import URI.ByteString

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Thentos.Action
import Thentos.Action.Core
import Thentos.Config
import Thentos.Frontend.Pages
import Thentos.Frontend.Handlers.Combinators
import Thentos.Frontend.State
import Thentos.Frontend.Types
import Thentos.Types

import qualified System.Log
import qualified System.Log.Missing
import qualified Thentos.Action.SimpleAuth as U
import qualified Thentos.Action.Unsafe as U


-- * helpers

-- FIXME these need to be replaced.

liftU :: U.UnsafeAction FActionError a -> FAction a
liftU = lift . U.unsafeAction

loggerF :: (Show v) => v -> FAction ()
loggerF = lift . loggerA

loggerA :: (Show v) => v -> Action FActionError ()
loggerA = U.unsafeAction . loggerU

loggerU :: (Show v) => v -> U.UnsafeAction FActionError ()
loggerU = U.logger System.Log.DEBUG . show


-- * forms

type HtmlForm (name :: Symbol) typ =
       FormGet HTM name H.Html typ
  :<|> FormPost HTM name H.Html typ :> Post '[HTM] H.Html

htmlForm :: (typ -> FAction Html) -> ServerT (HtmlForm name typ) FAction
htmlForm postHandler = return () :<|> postHandler


-- * register (thentos)

type UserRegisterH = "register" :> HtmlForm "UserRegister" UserFormData

instance HasForm "UserRegister" H.Html UserFormData where
    formAction _  = "/user/register"
    isForm _      = userRegisterForm
    formView _    = userRegisterPage "csrftok"
    formBackend _ = error "HasForm UserRegister formBackend: impossible"

userRegisterH :: ServerT UserRegisterH FAction
userRegisterH = htmlForm $ \userFormData -> do
    fcfg <- getFrontendConfig
    loggerF ("registering new user: " ++ show (udName userFormData))
    (_, tok) <- lift $ addUnconfirmedUser userFormData
    let url = emailConfirmUrl fcfg "/user/register_confirm" tok

    sendUserConfirmationMail userFormData url
    return userRegisterRequestedPage

sendUserConfirmationMail :: UserFormData -> ST -> FAction ()
sendUserConfirmationMail user callbackUrl = liftU $
    U.sendMail Nothing (udEmail user) subject message
  where
    message = "Please go to " <> callbackUrl <> " to confirm your account."
    subject = "Thentos account creation confirmation"

sendUserExistsMail :: UserEmail -> FAction ()
sendUserExistsMail address = liftU $
    U.sendMail Nothing address subject message
  where
    message = "Someone tried to sign up to Thentos with your email address"
                <> "\nThis is a reminder that you already have a Thentos"
                <> " account. If you haven't tried to sign up to Thentos, you"
                <> " can just ignore this email. If you have, you are hereby"
                <> " reminded that you already have an account."
    subject = "Attempted Thentos Signup"


type UserRegisterConfirmH = "register_confirm" :>
    QueryParam "token" ConfirmationToken :> Get '[HTM] Html

defaultUserRoles :: [Role]
defaultUserRoles = [RoleUser, RoleUserAdmin, RoleServiceAdmin]

userRegisterConfirmH :: ServerT UserRegisterConfirmH FAction
userRegisterConfirmH Nothing = crash FActionErrorNoToken
userRegisterConfirmH (Just token) = do
    (uid, sessTok) <- lift $ do
        loggerA $ "received user register confirm token: " ++ show token
        (_uid, _sessTok) <- confirmNewUser token
        loggerA $ "registered new user: " ++ show _uid
        grantAccessRights'P [RoleAdmin]
        mapM_ (assignRole (UserA _uid)) $ defaultUserRoles
        return (_uid, _sessTok)

    sendFrontendMsg $ FrontendMsgSuccess "Registration complete.  Welcome to Thentos!"
    userFinishLogin (uid, sessTok)


-- * login (thentos)

type UserLoginH = "login" :> HtmlForm "UserLogin" (UserName, UserPass)

instance HasForm "UserLogin" H.Html (UserName, UserPass) where
    formAction _  = "/user/login"
    isForm _      = userLoginForm
    formView _    = userLoginPage "csrftok"
    formBackend _ = error "HasForm UserLogin formBackend: impossible"

userLoginH :: ServerT UserLoginH FAction
userLoginH = htmlForm $ \(uname, passwd) -> do
    sendFrontendMsg $ FrontendMsgSuccess "Login successful.  Welcome to Thentos!"
    (lift (startThentosSessionByUserName uname passwd) >>= userFinishLogin)
        `catchError` \case
            BadCredentials -> do
                sendFrontendMsgs [FrontendMsgError "Bad username or password."]
                redirectRR $ RelativeRef Nothing "/user/login" (Query []) Nothing
            e -> throwError e

-- | If action yields uid and session token, login.  Otherwise, redirect to login page with a
-- message that asks to try again.
userFinishLogin :: (UserId, ThentosSessionToken) -> FAction Html
userFinishLogin (uid, tok) = do
    modify $ fsdLogin .~ Just (FrontendSessionLoginData tok uid)
    redirectToDashboardOrService



-- next: redirects seem to not work at all, but crash..  why?



-- * forgot password

{-
resetPassword :: FH ()
resetPassword = do
    runPageForm resetPasswordForm resetPasswordPage $ \ address -> do
        config :: ThentosConfig <- gets (^. cfg)
        feConfig <- gets (^. frontendCfg)
        eToken   <- snapRunActionE $ addPasswordResetToken address
        case eToken of
            Right (user, token) -> do
                snapRunAction $ sendPasswordResetMail
                    (Tagged $ config >>. (Proxy :: Proxy '["smtp"])) user
                    (emailConfirmUrl feConfig "/user/reset_password" token)
                blaze resetPasswordRequestedPage
            Left (ActionErrorThentos NoSuchUser) -> blaze resetPasswordRequestedPage
            Left e -> crash $ FActionError500 ("resetPassword" :: ST, e)

sendPasswordResetMail :: SmtpConfig -> User -> ST -> Action Void ()
sendPasswordResetMail smtpConfig user callbackUrl = do
    sendMail'P smtpConfig Nothing (user ^. userEmail) subject message
  where
    message = "To set a new password, go to " <> callbackUrl
    subject = "Thentos Password Reset"

resetPasswordConfirm :: FH ()
resetPasswordConfirm = do
    mToken <- (>>= urlDecode) <$> getParam "token"
    let meToken = PasswordResetToken <$$> decodeUtf8' <$> mToken

    runPageForm resetPasswordConfirmForm resetPasswordConfirmPage $ \ password -> case meToken of
        -- process reset form input
        (Just (Right token)) -> do
            eResult <- snapRunActionE $ A.resetPassword token password
            case eResult of
                Right () -> do
                    sendFrontendMsg $ FrontendMsgSuccess "Password changed successfully.  Welcome back to Thentos!"

                    -- FIXME: what we would like to do here is login
                    -- the user right away:
                    --
                    -- >>> userLoginCallAction $ (uid,) <$> startSessionNoPass (UserA uid)
                    --
                    -- But we need the uid for that, and we need to
                    -- find it under the confirmation token in the DB
                    -- (taking it from the request would be insecure!)

                    redirect' "/dashboard" 303
                Left (ActionErrorThentos NoSuchToken) -> crash FActionErrorNoToken
                Left e -> do
                    crash $ FActionError500 "Change password: error."

        -- error cases
        (Just (Left _)) -> crash FActionErrorNoToken
        Nothing         -> crash FActionErrorNoToken


-- * logout (thentos)

userLogout :: FH ()
userLogout = method GET  userLogoutConfirm
         <|> method POST userLogoutDone

userLogoutConfirm :: FH ()
userLogoutConfirm = runAsUser $ \ _ fsl -> do
    eServiceNames <- snapRunActionE $ serviceNamesFromThentosSession (fsl ^. fslToken)
    tok <- with sess csrfToken
    case eServiceNames of
        Right serviceNames -> renderDashboard DashboardTabLogout (userLogoutConfirmPagelet "/user/logout" serviceNames tok)
        Left e -> crash $ FActionError500 e

userLogoutDone :: FH ()
userLogoutDone = runAsUser $ \ _ fsl -> do
    snapRunAction $ endThentosSession (fsl ^. fslToken)
    modifySessionData' $ fsdLogin .~ Nothing
    blaze userLogoutDonePage


-- * user update

emailUpdate :: FH ()
emailUpdate = runAsUser $ \ _ fsl -> do
    tok <- with sess csrfToken
    runPageletForm emailUpdateForm
                   (emailUpdatePagelet tok) DashboardTabDetails
                   $ \ newEmail -> do
        feConfig <- gets (^. frontendCfg)
        eResult <- snapRunActionE $
            requestUserEmailChange (fsl ^. fslUserId) newEmail
                (emailConfirmUrl feConfig "/user/update_email_confirm")
        case eResult of
            Right ()                                         -> emailSent
            Left (ActionErrorThentos UserEmailAlreadyExists) -> emailSent
            Left e                                           -> crash $ FActionError500 e
  where
    emailSent = do
        sendFrontendMsgs $
            FrontendMsgSuccess "Your new email address has been stored." :
            FrontendMsgSuccess "It will be activated once you process the confirmation email." :
            []
        redirect' "/dashboard" 303

emailUpdateConfirm :: FH ()
emailUpdateConfirm = do
    mToken <- (>>= urlDecode) <$> getParam "token"
    let meToken = ConfirmationToken <$$> decodeUtf8' <$> mToken
    case meToken of
        Just (Right token) -> do
            eResult <- snapRunActionE $ confirmUserEmailChange token
            case eResult of
                Right ()          -> sendFrontendMsg (FrontendMsgSuccess "Change email: success!") >> redirect' "/dashboard" 303
                Left (ActionErrorThentos NoSuchToken)
                                  -> crash FActionErrorNoToken
                Left e            -> crash $ FActionError500 e
        Just (Left _unicodeError) -> crash FActionErrorNoToken
        Nothing                   -> crash FActionErrorNoToken

passwordUpdate :: FH ()
passwordUpdate = runAsUser $ \ _ fsl -> do
    tok <- with sess csrfToken
    runPageletForm passwordUpdateForm
                   (passwordUpdatePagelet tok) DashboardTabDetails
                   $ \ (oldPw, newPw) -> do
        eResult <- snapRunActionE $ changePassword (fsl ^. fslUserId) oldPw newPw
        case eResult of
            Right () -> sendFrontendMsg (FrontendMsgSuccess "Change password: success!") >> redirect' "/dashboard" 303
            Left (ActionErrorThentos BadCredentials)
                     -> sendFrontendMsg (FrontendMsgError "Invalid old password.") >> redirect' "/user/update_password" 303
            Left e   -> crash $ FActionError500 e


-- * services

serviceCreate :: FH ()
serviceCreate = runAsUser $ \ _ fsl -> do
    tok <- with sess csrfToken
    runPageletForm serviceCreateForm
                   (serviceCreatePagelet tok) DashboardTabOwnServices
                   $ \ (name, description) -> do
        eResult <- snapRunActionE $ addService (fsl ^. fslUserId) name description
        case eResult of
            Right (sid, key) -> do
                sendFrontendMsgs
                    [ FrontendMsgSuccess "Added a service!"
                    , FrontendMsgSuccess $ "Service id: " <> fromServiceId sid
                    , FrontendMsgSuccess $ "Service key: " <> fromServiceKey key
                    ]
                redirect' "/dashboard" 303
            Left e -> logger INFO (show e) >> crash 400 "Create service: failed."

-- | (By the time this handler is called, serviceLogin has to have been
-- called so we have a callback to the login page stored in the
-- session state.)
serviceRegister :: FH ()
serviceRegister = runAsUser $ \ _ fsl -> do
    ServiceLoginState sid rr <- getServiceLoginState >>= maybe (crash $ FActionError500 "Service login: no state.") return

    let present :: ST -> View H.Html -> FH ()
        present formAction view = do
            (_, user)    <- snapRunAction (lookupConfirmedUser (fsl ^. fslUserId))
            (_, service) <- snapRunActionE (lookupService sid)
                        >>= either (\ e -> crash $ FActionError500 (e, "Service registration: misconfigured service (no service id).")) return
            -- FIXME: we are doing a lookup on the service table, but
            -- the service may have an opinion on whether the user is
            -- allowed to look it up.  the user needs to present a
            -- cryptographic proof of the service's ok for lookup
            -- here.
            tok <- with sess csrfToken
            blaze $ serviceRegisterPage tok formAction view sid service user

        process :: () -> FH ()
        process () = do
            eResult <- snapRunActionE $ addServiceRegistration (fsl ^. fslToken) sid
            case eResult of
                Right () -> redirectRR rr
                -- (We match the '()' explicitly here just
                -- because we can, and because nobody has to
                -- wonder what's hidden in the '_'.  No
                -- lazyless counter-magic is involved.)

                Left e -> crash $ FActionError500 (e, "Service registration: error.")
                -- ("Unknown service id" should have been
                -- caught in the "render form" case.  Perhaps
                -- the user has tampered with the cookie?)

    runHandlerForm serviceRegisterForm present process

-- | Coming from a service site, handle the authentication and
-- redirect to service with valid session token.  This may happen in a
-- series of redirects through the thentos frontend; the state of this
-- series is stored in `fsdServiceLoginState`.
--
-- FIXME[mf] (thanks to Sönke Hahn): The session token seems to be
-- contained in the url. So if people copy the url from the address
-- bar and send it to someone, they will get the same session.  The
-- session token should be in a cookie, shouldn't it?
serviceLogin :: FH ()
serviceLogin = do
    ServiceLoginState sid _ <- setServiceLoginState

    let -- case A: user is not logged into thentos.  we have stored
        -- service login callback already at this point, so just
        -- redirect to login page.
        notLoggedIn :: FH ()
        notLoggedIn = redirect' "/user/login" 303

        loggedIn :: FrontendSessionLoginData -> FH ()
        loggedIn fsl = do
            let tok = fsl ^. fslToken
            eSessionToken :: Either (ActionError Void) ServiceSessionToken
                <- snapRunActionE $ startServiceSession tok sid

            case eSessionToken of
                -- case B: user is logged into thentos and registered
                -- with service.  clean up the 'ServiceLoginState'
                -- stash in thentos session state, extract the
                -- callback URI from the request parameters, inject
                -- the session token we just created, and redirect.
                Right (ServiceSessionToken sessionToken) -> do
                    _ <- popServiceLoginState
                    let f = uriQueryL . queryPairsL %~ (("token", cs sessionToken) :)
                    meCallback <- parseURI laxURIParserOptions <$$> getParam "redirect"
                    case meCallback of
                        Just (Right callback) -> redirectURI $ f callback
                        Just (Left _)         -> crash FActionErrorServiceLoginNoCallbackUrl
                        Nothing               -> crash FActionErrorServiceLoginNoCallbackUrl

                -- case C: user is logged into thentos, but not
                -- registered with service.  redirect to service
                -- registration page.
                Left (ActionErrorThentos NotRegisteredWithService) -> do
                    redirect' "/service/register" 303

                -- case D: user is logged into thentos, but something
                -- unexpected went wrong (possibly the session was
                -- corrupted by the user/adversary?).  report error to
                -- log file and user.
                Left e -> do
                    crash $ FActionError500 "Service login: could not initiate session."

    runAsUserOrNot (\ _ -> loggedIn) notLoggedIn
-}


-- | If a service login state exists, consume it, jump back to the
-- service, and log in.  If not, jump to `/dashboard`.
redirectToDashboardOrService :: FAction H.Html
redirectToDashboardOrService = do
    mCallback <- popServiceLoginState
    case mCallback of
        Just (ServiceLoginState _ rr) -> redirectRR rr
        Nothing                       -> redirect' "/dashboard"


-- * Cache control

{-

-- | Disable response caching. The wrapped handler can overwrite this by
-- setting its own cache control headers.
--
-- Cache-control headers are only added to GET and HEAD responses since other request methods
-- are considered uncachable by default.
--
-- Note that, though this handler is always called, its actions are sometimes discarded by Snap,
-- e.g. if the 'error' function is called. That leads to an 500 Internal Server Error responses
-- *without* the additional headers added by this handler. This may not be so bad since (a)
-- we don't want to return any error 500 pages and (b) they are considered uncacheable anyway.
-- But it's something to keep in mind.
--
-- According to the HTTP 1.1 Spec, GET/HEAD responses with the following error codes (>= 400) may
-- be cached unless forbidded by cache-control headers:
--
-- * 404 Not Found
-- * 405 Method Not Allowed
-- * 410 Gone
-- * 414 Request-URI Too Long
-- * 501 Not Implemented
--
-- The 'unknownPath' handler takes care of 404 responses. The other cacheable response types will
-- probably rarely be generated by Snap, but we should keep an eye on them.
disableCaching :: Handler b v a -> Handler b v a
disableCaching h = do
    req <- getRequest
    when (rqMethod req `elem` [GET, HEAD]) addCacheControlHeaders
    h
  where
    addCacheControlHeaders =
        modifyResponse $ setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
                       . setHeader "Expires" "0"

-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Thentos.Frontend.Handlers
  ( UserRegisterH
  , UserRegisterConfirmH
  , UserLoginH
  , ResetPasswordRequestH
  , ResetPasswordH
  , UserLogoutH
  , EmailUpdateH
  , EmailUpdateConfirmH
  , PasswordUpdateH
  , DashboardH
  , ServiceCreateH
  , ServiceRegisterH
  , ServiceLoginH

  , userRegisterH
  , userRegisterConfirmH
  , userLoginH
  , resetPasswordRequestH
  , resetPasswordH
  , userLogoutH
  , emailUpdateH
  , emailUpdateConfirmH
  , passwordUpdateH
  , dashboardH
  , serviceCreateH
  , serviceRegisterH
  , serviceLoginH

  , disableCaching
  )
where

import Control.Arrow (first)
import Control.Lens ((.~), (^.))
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (get, modify)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, (<>))
import LIO.DCLabel (toCNF)
import Network.Wai.Parse (Param, parseRequestBody, lbsBackEnd)
import Network.Wai (Request, Middleware, requestMethod)
import Servant (QueryParam, (:<|>)((:<|>)), (:>), ServerT)
import Servant.Server.Internal (HasServer, Router'(WithRequest), RouteResult(Route),
                                route, addBodyCheck)
import Text.Digestive (Env, Form, FormInput(TextInput), View, fromPath, getForm, postForm)

import qualified Servant
import qualified Text.Blaze.Html5 as H
import qualified Data.Text.Encoding as STE

import Thentos.Action
import Thentos.Action.Core
import Thentos.Backend.Core (addHeadersToResponse)
import Thentos.Ends.Types
import Thentos.Frontend.Handlers.Combinators
import Thentos.Frontend.Pages
import Thentos.Frontend.State
import Thentos.Frontend.Types
import Thentos.Types

import qualified Thentos.Action.SimpleAuth as U
import qualified Thentos.Action.Unsafe as U


-- * helpers

type Get = Servant.Get '[HTM] H.Html
type Post = Servant.Post '[HTM] H.Html

type FormH payload = Get :<|> FormReqBody :> Post

data FormReqBody

instance (HasServer sublayout) => HasServer (FormReqBody :> sublayout) where
  type ServerT (FormReqBody :> sublayout) m = Env Identity -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \request ->
      route (Proxy :: Proxy sublayout) (addBodyCheck subserver (bodyCheck request))
    where
      -- FIXME: honor accept header
      -- FIXME: file upload.  shouldn't be hard!
      bodyCheck :: Request -> IO (RouteResult (Env Identity))
      bodyCheck req = do
          q :: [Param]
              <- parseRequestBody lbsBackEnd req >>=
                  \case (q, []) -> return q
                        (_, _:_) -> error "servant-digestive-functors: file upload not implemented!"

          let env :: Env Identity
              env query = return
                        . map (TextInput . STE.decodeUtf8 . snd)
                        . filter ((== fromPath query) . fst)
                        . map (first STE.decodeUtf8)
                        $ q

          return $ Route env


-- | There are two form processor functions that are combined in the execution: the
-- digestive-functors part that composes the form payload or a list of errors to be sent back to the
-- browser, and the part that has effects on system and session state.  The first is factored out so
-- it can live in the Safe "Pages" module.  The renderer needs to be an action so it can e.g. flush
-- and render the 'FrontendMsg' queue.
--
-- FIXME: why does processor I have 'FAction' in its type?
-- FIXME: replace H.Html in processor2 and renderer with @(MimeRender 'HTML a => a)@ (or something).
-- FIXME: move this to servant-digestive-functors.
formH :: forall payload.
     ST                                     -- ^ formAction
  -> Form H.Html FAction payload            -- ^ processor I
  -> (payload -> FAction H.Html)            -- ^ processor II
  -> (View H.Html -> ST -> FAction H.Html)  -- ^ renderer
  -> ServerT (FormH payload) FAction
formH formAction processor1 processor2 renderer = getH :<|> postH
  where
    getH = do
        v <- getForm formAction processor1
        H.toHtml <$> renderer v formAction

    postH :: Env Identity -> FAction H.Html
    postH env = postForm formAction processor1 (\_ -> return $ return . runIdentity . env) >>=
        \case (_,                Just payload) -> processor2 payload
              (v :: View H.Html, Nothing)      -> renderer v formAction


-- * register (thentos)

type UserRegisterH = "register" :> FormH UserFormData

userRegisterH :: ServerT UserRegisterH FAction
userRegisterH = formH "/user/register" userRegisterForm p (showPageWithMessages userRegisterPage)
  where
    p :: UserFormData -> FAction H.Html
    p userFormData = do
        loggerF ("registering new user: " ++ show (udName userFormData))
        addUnconfirmedUser userFormData
        -- BUG #402: the frontend expects "/user/register_confirm/<ConfirmationToken>",
        -- but the link is now generated in the backend and has the form
        -- "/activate/<ConfirmationToken>"

        -- FIXME: there could be a nicer reaction if the user attempts to use a nick that
        -- is already in use.
        userRegisterRequestedPage <$> get


type UserRegisterConfirmH = "register_confirm" :>
    QueryParam "token" ConfirmationToken :> Get

defaultUserRoles :: [Role]
defaultUserRoles = [RoleUser, RoleUserAdmin, RoleServiceAdmin]

userRegisterConfirmH :: ServerT UserRegisterConfirmH FAction
userRegisterConfirmH Nothing = crash FActionErrorNoToken
userRegisterConfirmH (Just token) = do
    (uid, sessTok) <- do
        loggerF $ "received user register confirm token: " ++ show token
        (_uid, _sessTok) <- confirmNewUser token
        loggerF $ "registered new user: " ++ show _uid
        -- FIXME: we need a 'withAccessRights' for things like this.
        grantAccessRights'P [toCNF RoleAdmin]
        mapM_ (assignRole (UserA _uid)) $ defaultUserRoles
        return (_uid, _sessTok)

    sendFrontendMsg $ FrontendMsgSuccess "Registration complete.  Welcome to Thentos!"
    userFinishLogin (uid, sessTok)


-- * login (thentos)

type UserLoginH = "login" :> FormH (UserName, UserPass)

userLoginH :: ServerT UserLoginH FAction
userLoginH = formH "/user/login" userLoginForm p (showPageWithMessages userLoginPage)
  where
    p :: (UserName, UserPass) -> FAction H.Html
    p (uname, passwd) =
        (startThentosSessionByUserName uname passwd >>= userFinishLogin)
            `catchError` userFailLogin

-- | If action yields uid and session token, login.  Otherwise, redirect to login page with a
-- message that asks to try again.
userFinishLogin :: (UserId, ThentosSessionToken) -> FAction H.Html
userFinishLogin (uid, tok) = do
    modify $ fsdLogin .~ Just (FrontendSessionLoginData tok uid (Just DashboardTabDetails))
    sendFrontendMsg $ FrontendMsgSuccess "Login successful.  Welcome to Thentos!"
    redirectToDashboardOrService

userFailLogin :: ThentosError FActionError -> FAction H.Html
userFailLogin BadCredentials = do
    sendFrontendMsgs [FrontendMsgError "Bad username or password."]
    redirect' "/user/login"
userFailLogin e = throwError e


-- * forgot password

type ResetPasswordRequestH = "reset_password_request" :> FormH UserEmail

resetPasswordRequestH :: ServerT ResetPasswordRequestH FAction
resetPasswordRequestH =
    formH "/user/reset_password_request" resetPasswordRequestForm p
                                (showPageWithMessages resetPasswordRequestPage)
  where
    p :: UserEmail -> FAction H.Html
    p uemail = do
        fcfg <- getFrontendCfg
        loggerF ("password reset request: " ++ show uemail)
        (do
            (user, token) <- addPasswordResetToken uemail
            let url = emailConfirmUrl fcfg "/user/reset_password" (fromPasswordResetToken token)
            sendPasswordResetMail user url
            resetPasswordRequestedPage <$> get)
          `catchError` \case
            NoSuchUser -> resetPasswordRequestedPage <$> get  -- FIXME: send out warning, too?
            e -> throwError e

sendPasswordResetMail :: User -> ST -> Action FActionError FrontendSessionData ()
sendPasswordResetMail user callbackUrl = U.unsafeAction $ do
    U.sendMail Nothing (user ^. userEmail) subject message
  where
    message = "To set a new password, go to " <> callbackUrl
    subject = "Thentos Password Reset"


type ResetPasswordH =
      "reset_password" :> QueryParam "token" PasswordResetToken :> FormH UserPass

resetPasswordH :: ServerT ResetPasswordH FAction
resetPasswordH mTok = formH "/usr/reset_password" resetPasswordForm (p mTok)
                                            (showPageWithMessages resetPasswordPage)
  where
    p :: Maybe PasswordResetToken -> UserPass -> FAction H.Html
    p Nothing _ = crash FActionErrorNoToken
    p (Just tok) password = do
        resetPassword tok password
        sendFrontendMsg $ FrontendMsgSuccess "Password changed successfully.  Welcome back to Thentos!"

        -- FIXME: what we would like to do here is login the user right away, with something like
        --
        -- >>> userLoginCallAction $ (uid,) <$> startSessionNoPass (UserA uid)
        --
        -- this requires that 'resetPassword' returns the 'UserId' that we need for login, so for
        -- now the user is force to do it manually, might be marginally more secure, too.
        redirect' "/dashboard"


-- * logout (thentos)

type UserLogoutH = "logout" :> (Get :<|> Post)

userLogoutH :: ServerT UserLogoutH FAction
userLogoutH = userLogoutConfirmH :<|> userLogoutDoneH

userLogoutConfirmH :: FAction H.Html
userLogoutConfirmH = runAsUserOrLogin $ \_ fsl -> do
    serviceNames <- serviceNamesFromThentosSession (fsl ^. fslToken)
    -- BUG #403: do we need csrf protection for this?
    setTab DashboardTabLogout
    renderDashboard (userLogoutConfirmSnippet "/user/logout" serviceNames "csrfToken")

userLogoutDoneH :: FAction H.Html
userLogoutDoneH = runAsUserOrLogin $ \_ fsl -> do
    endThentosSession (fsl ^. fslToken)
    modify $ fsdLogin .~ Nothing
    userLogoutDonePage <$> get


-- * user update

type EmailUpdateH = "update_email" :> FormH UserEmail

-- BUG #403: csrf?
emailUpdateH :: ServerT EmailUpdateH FAction
emailUpdateH = formH "/user/reset_password_request" emailUpdateForm p r
  where
    r v a = do
        setTab DashboardTabDetails
        fsd <- get
        renderDashboard $ emailUpdateSnippet fsd v a

    p :: UserEmail -> FAction H.Html
    p uemail = do
        loggerF ("email change request: " ++ show uemail)
        fcfg <- getFrontendCfg
        let go = do
              runAsUserOrLogin $ \_ fsl -> requestUserEmailChange (fsl ^. fslUserId) uemail
                      $ emailConfirmUrl fcfg "/user/update_email_confirm" . fromConfirmationToken
              emailSent

        go `catchError`
            \case UserEmailAlreadyExists -> emailSent
                  e                      -> throwError e

    emailSent = do
        sendFrontendMsgs $
            FrontendMsgSuccess "Your new email address has been stored." :
            FrontendMsgSuccess "It will be activated once you process the confirmation email." :
            []
        redirect' "/dashboard"


type EmailUpdateConfirmH = "update_email_confirm" :>
    QueryParam "token" ConfirmationToken :> Get

emailUpdateConfirmH :: ServerT EmailUpdateConfirmH FAction
emailUpdateConfirmH Nothing = crash FActionErrorNoToken
emailUpdateConfirmH (Just token) = go `catchError`
      \case NoSuchToken -> crash FActionErrorNoToken
            e           -> throwError e
  where
    go = do
        confirmUserEmailChange token
        sendFrontendMsg (FrontendMsgSuccess "Change email: success!")
        redirect' "/dashboard"


type PasswordUpdateH = "update_password" :> FormH (UserPass, UserPass)

passwordUpdateH :: ServerT PasswordUpdateH FAction
passwordUpdateH = formH "/user/update_password" p1 p2 r
  where
    r v a = do
        setTab DashboardTabDetails
        fsd <- get
        renderDashboard $ passwordUpdateSnippet fsd v a

    p1 = passwordUpdateForm

    p2 :: (UserPass, UserPass) -> FAction H.Html
    p2 (oldPass, newPass) = do
        loggerF ("password change request." :: String)
        let go = runAsUserOrLogin $ \_ fsl -> changePassword (fsl ^. fslUserId) oldPass newPass
            worked = sendFrontendMsg (FrontendMsgSuccess "Change password: success!")
                >> redirect' "/dashboard"
            didn't = sendFrontendMsg (FrontendMsgError "Invalid old password.")
                >> redirect' "/user/update_password"

        (go >> worked) `catchError`
          \case BadCredentials -> didn't
                e              -> throwError e


-- * dashboard

type DashboardH =
       Get
  :<|> "details"     :> Get
  :<|> "services"    :> Get
  :<|> "ownservices" :> Get
  :<|> "users"       :> Get

dashboardH :: ServerT DashboardH FAction
dashboardH =
       redirect' "/dashboard/details"
  :<|> (setTab DashboardTabDetails  >> renderDashboard userDisplaySnippet)
  :<|> (setTab DashboardTabServices >> renderDashboard userServicesDisplaySnippet)
  :<|> redirect' "/service/create"
  :<|> (setTab DashboardTabUsers    >> renderDashboard (\ _ _ -> "nothing here yet!"))


-- * services

-- FIXME: this route should be something more like @/service/create@.
type ServiceCreateH = "create" :> FormH (ServiceName, ServiceDescription)

-- BUG #403: csrf
serviceCreateH :: ServerT ServiceCreateH FAction
serviceCreateH = formH "/service/create" serviceCreateForm p r
  where
    r v a = do
        -- (FIXME: the whole way tabs are switched could use a bit more work.)
        setTab DashboardTabDetails
        fsd <- get
        renderDashboard $ serviceCreateSnippet fsd v a

    p :: (ServiceName, ServiceDescription) -> FAction H.Html
    p (name, description) = do
        loggerF ("service creation request." :: String)
        (sid, key) <- runAsUserOrLogin $ \_ fsl -> addService (fsl ^. fslUserId) name description
        sendFrontendMsgs
            [ FrontendMsgSuccess "Added a service!"
            , FrontendMsgSuccess $ "Service id: " <> fromServiceId sid
            , FrontendMsgSuccess $ "Service key: " <> fromServiceKey key
            ]
        redirect' "/dashboard"


type ServiceRegisterH = "register" :> FormH ()

-- BUG #403: csrf

-- FIXME: security: we are doing a lookup on the service table, but the service may have an opinion
-- on whether the user is allowed to look it up.  the user needs to present a cryptographic proof of
-- the service's ok for lookup here.

serviceRegisterH :: ServerT ServiceRegisterH FAction
serviceRegisterH = formH "/service/register" serviceRegisterForm (\() -> p) r
  where
    r v a = do
        fsd <- get
        ServiceLoginState sid _ <- getServiceLoginState
        (_, service) <- lookupService sid
        (_, user) <- runAsUserOrLogin $ \_ fsl -> lookupConfirmedUser $ fsl ^. fslUserId
        pure $ serviceRegisterPage fsd v a sid service user

    p :: FAction H.Html
    p = do
        ServiceLoginState sid rr <- getServiceLoginState
        runAsUserOrLogin $ \_ fsl -> addServiceRegistration (fsl ^. fslToken) sid
        redirectRR rr


type ServiceLoginH = "login"
                  :> QueryParam "serviceId" ServiceId
                  :> QueryParam "redirect" RelRef
                  :> Get

-- | Coming from a service site, handle the authentication and redirect to service with valid
-- session token.  This may happen in a series of redirects through the thentos frontend; the state
-- of this series is stored in `fsdServiceLoginState`.  The control flow in detail:
--
-- *case A:* user is not logged into thentos.  we have stored service login callback already at this
-- point, so just redirect to login page.
--
-- *case B:* user is logged into thentos and registered with service.  clean up the
-- 'ServiceLoginState' stack, inject the service session token just created into the redirect uri,
-- and redirect.
--
-- *case C:* user is logged into thentos, but not registered with service.  redirect to service
-- registration page.
--
-- BUG #404: Sönke Hahn: "The session token seems to be contained in the url. So if people copy the
-- url from the address bar and send it to someone, they will get the same session.  The session
-- token should be in a cookie, shouldn't it?"  (We will use some SSO protocol here that is not home
-- cooked later; for prototype operations, this is not serious.)
serviceLoginH :: Maybe ServiceId -> Maybe RelRef -> FAction a
serviceLoginH Nothing _ = crash $ FActionError500 "Service login: no Service ID."
serviceLoginH _ Nothing = crash $ FActionError500 "Service login: no or malformed redirect URI"
serviceLoginH (Just sid) (Just (RelRef rr)) = do
    let sls = ServiceLoginState sid rr
    loggerF $ "setServiceLoginState: " ++ show sls
    modify $ fsdServiceLoginState .~ Just sls

    runAsUserOrLogin $ \_ fsl -> do
        -- BUG #405: the token needs to be stored in the query of the redirect url.
        ServiceSessionToken _ <- startServiceSession (fsl ^. fslToken) sid
          `catchError` \case NotRegisteredWithService
                               -> redirect' "/service/register"
                             e -> throwError e
        _ <- popServiceLoginState
        redirectRR rr


-- | If a service login state exists, consume it, jump back to the
-- service, and log in.  If not, jump to `/dashboard`.
redirectToDashboardOrService :: FAction H.Html
redirectToDashboardOrService = do
    mCallback <- popServiceLoginState
    case mCallback of
        Just (ServiceLoginState _ rr) -> redirectRR rr
        Nothing                       -> redirect' "/dashboard"


-- * Cache control

-- | Disable response caching. The wrapped handler can overwrite this by
-- setting its own cache control headers.
--
-- Cache-control headers are only added to GET and HEAD responses since other request methods
-- are considered uncachable by default.
--
-- According to the HTTP 1.1 Spec, GET/HEAD responses with the following error codes (>= 400) may
-- be cached unless forbidded by cache-control headers:
--
-- * 404 Not Found
-- * 405 Method Not Allowed
-- * 410 Gone
-- * 414 Request-URI Too Long
-- * 501 Not Implemented
disableCaching :: Middleware
disableCaching app req cont = app req $
    cont . (if relevantMeth then addHeadersToResponse cacheHeaders else id)
  where
    cacheHeaders =
        [ ("Cache-Control", "no-cache, no-store, must-revalidate")
        , ("Expires", "0")
        ]

    relevantMeth :: Bool
    relevantMeth = requestMethod req `elem` ["GET", "HEAD"]

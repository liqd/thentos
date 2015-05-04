{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}

module Thentos.Frontend.Handlers where

import Control.Applicative ((<$>), (<|>))
import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.), (.~), (%~))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)
import Crypto.Random (SystemRNG)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Functor.Infix ((<$$>))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs)
import Data.Text.Encoding (decodeUtf8')
import LIO.DCLabel ((/\), (\/))
import Snap.Blaze (blaze)
import Snap.Core (Method(GET, POST), method)
import Snap.Core (getParam, redirect', modifyResponse, setResponseStatus)
import Snap.Core (urlDecode)
import Snap.Snaplet.AcidState (update)
import System.Log.Missing (logger)
import System.Log (Priority(DEBUG, INFO, WARNING, CRITICAL))
import Text.Digestive.View (View)
import URI.ByteString (parseURI, laxURIParserOptions, uriQueryL, queryPairsL)
import URI.ByteString (RelativeRef(..), Query(..))

import qualified Text.Blaze.Html5 as H

import Thentos.Api
import Thentos.Config
import Thentos.DB
import Thentos.Frontend.Pages
import Thentos.Frontend.Types
import Thentos.Frontend.Util
import Thentos.Smtp
import Thentos.Types


-- * register (thentos)

userRegister :: FH ()
userRegister = do
    let clearance = RoleOwnsUnconfirmedUsers *%% RoleOwnsUnconfirmedUsers
    runPageForm userRegisterForm userRegisterPage $
            \ (userFormData :: UserFormData) -> do
        result' <- snapRunAction' clearance $ addUnconfirmedUser userFormData
        case result' of
            Right (_, token) -> do
                config :: ThentosConfig <- gets (^. cfg)
                feConfig <- gets (^. frontendCfg)
                liftIO $ sendUserConfirmationMail
                    (Tagged $ config >>. (Proxy :: Proxy '["smtp"])) userFormData
                    (urlConfirm feConfig "/user/register_confirm" (fromConfirmationToken token))
                blaze userRegisterRequestedPage
            Left UserEmailAlreadyExists -> do
                config :: ThentosConfig <- gets (^. cfg)
                liftIO $ sendUserExistsMail (Tagged $ config >>. (Proxy :: Proxy '["smtp"])) (udEmail userFormData)
                blaze userRegisterRequestedPage
            Left e -> logger INFO (show e) >> crash 400 "Registration failed."

defaultUserRoles :: [Role]
defaultUserRoles = RoleBasic <$> [RoleUser, RoleUserAdmin, RoleServiceAdmin]

userRegisterConfirm :: FH ()
userRegisterConfirm = do
    let clearance = RoleOwnsUnconfirmedUsers /\ RoleOwnsUsers *%% RoleOwnsUnconfirmedUsers \/ RoleOwnsUsers

    mTokenBS <- getParam "token"
    case ConfirmationToken <$$> (decodeUtf8' <$> mTokenBS) of
        Just (Right token) -> do
            eResult <- snapRunAction' allowEverything $ confirmNewUser token
                                     -- FIXME: authorization
            case eResult of
                Right uid -> do
                    logger DEBUG $ "registered new user: " ++ show uid
                    userLoginCallAction $ (uid,) <$> startSessionNoPass (UserA uid)
                    logger DEBUG $ "registered new user: session started."
                    mapM_ (\ r -> snapRunAction' allowEverything . updateAction
                                    $ AssignRole (UserA uid) r) defaultUserRoles
                                  -- FIXME: clearance level is too high, right?
                    logger DEBUG $ "registered new user: added default roles."
                    sendFrontendMsg $ FrontendMsgSuccess "Registration complete.  Welcome to Thentos!"
                    redirectToDashboardOrService
                Left e@NoSuchPendingUserConfirmation -> do
                    logger INFO $ show e
                    crash 400 "Finalizing registration failed: unknown token."
                Left e -> do
                    logger CRITICAL $ "unreachable: " ++ show e
                    crash 400 "Finializing registration failed."
        Just (Left unicodeError) -> do
            crash' 400 unicodeError "Bad user confirmation link."
        Nothing -> do
            crash 400 "No confirmation token."


-- * login (thentos)

userLogin :: FH ()
userLogin = do
    mMsg :: Maybe ST <- cs <$$> getParam "error_msg"
    runPageForm userLoginForm (userLoginPage mMsg) $ \ (username, password) -> do
        sendFrontendMsg $ FrontendMsgSuccess "Login successful.  Welcome to Thentos!"
        userLoginCallAction $ startThentosSessionByUserName username password


-- | If user name and password match, login.  Otherwise, redirect to
-- login page with a message that asks to try again.
userLoginCallAction :: Action (MVar SystemRNG) (UserId, SessionToken) -> FH ()
userLoginCallAction action = do
    result <- snapRunAction' allowEverything action
      -- FIXME[mf]: See 'runThentosUpdateWithLabel' in
      -- "Thentos.DB.Core".  Use that to create transaction
      -- 'CheckPasswordWithLabel', then call that with
      -- 'allowNothing' and 'thentosPublic'.
    case result of
        Right (uid, sessionToken) -> do
            modifySessionData' $ fsdLogin .~ Just (FrontendSessionLoginData sessionToken uid)
            redirectToDashboardOrService
        Left BadCredentials -> redirectRR
            (RelativeRef Nothing "/user/login" (Query [("error_msg", "Bad username or password.")]) Nothing)
                  -- FIXME: this error passing method has been
                  -- deprecated by the 'FrontendMsg' queue in the snap
                  -- state.  almost, that is: currently, messages are
                  -- only displayed inside the dashboard, but this
                  -- here is before login.  anyway, there should be
                  -- one way of doing this, not two.
        Left _ -> error "logIntoThentosHandler: branch should not be reachable"
            -- FIXME: this should be handled.  we should
            -- always allow transactions / actions to throw
            -- errors.


-- * forgot password

resetPasswordRequest :: FH ()
resetPasswordRequest = do
    runPageForm resetPasswordRequestForm resetPasswordRequestPage $ \ address -> do
        config :: ThentosConfig <- gets (^. cfg)
        feConfig <- gets (^. frontendCfg)
        eToken <-
            snapRunAction' allowEverything $ addPasswordResetToken address
        case eToken of
            Left NoSuchUser -> blaze resetPasswordRequestedPage
            Right (user, token) -> do
                liftIO $ sendPasswordResetMail
                    (Tagged $ config >>. (Proxy :: Proxy '["smtp"])) user
                    (urlConfirm feConfig "/user/reset_password" (fromPasswordResetToken token))
                blaze resetPasswordRequestedPage
            Left _ -> error "requestPasswordResetHandler: unreached"

resetPassword :: FH ()
resetPassword = do
    mToken <- (>>= urlDecode) <$> getParam "token"
    let meToken = PasswordResetToken <$$> decodeUtf8' <$> mToken

    runPageForm resetPasswordForm resetPasswordPage $ \ password -> case meToken of
        -- process reset form input
        (Just (Right token)) -> do
            result <- snapRunAction' allowEverything $ Thentos.Api.resetPassword token password
            case result of
                Right () -> blaze $ "Password succesfully changed."
                Left NoSuchToken -> crash 400 "No such reset token."
                Left e -> do
                    logger WARNING (show e)
                    crash 400 "Change password: error."

        -- error cases
        (Just (Left _)) -> do
            let msg = "Bad request: bad reset token."
            modifyResponse $ setResponseStatus 400 (cs msg)
            blaze (H.text msg)
        (Nothing) -> do
            let msg = "Bad request: reset password, but no token."
            modifyResponse $ setResponseStatus 400 (cs msg)
            blaze (H.text msg)


-- * logout (thentos)

userLogout :: FH ()
userLogout = method GET  userLogoutConfirm
         <|> method POST userLogoutDone

userLogoutConfirm :: FH ()
userLogoutConfirm = runAsUser $ \ _ _ fsl -> do
    eServiceNames <- snapRunAction' allowEverything $ getSessionServiceNames (fsl ^. fslToken) (fsl ^. fslUserId)
    case eServiceNames of
        Right serviceNames -> renderDashboard DashboardTabLogout (userLogoutConfirmPagelet "/user/logout" serviceNames)
        Left e@NoSuchUser -> crash500 e
        Left e@NoSuchSession -> crash500 e
        Left _ -> logger CRITICAL "unreachable: userLogoutConfirm" >> crash500 () -- FIXME

userLogoutDone :: FH ()
userLogoutDone = runAsUser $ \ _ _ fsl -> do
    _ <- snapRunAction' allowEverything . updateAction $ EndSession (fsl ^. fslToken)
    modifySessionData' $ fsdLogin .~ Nothing
    blaze userLogoutDonePage


-- * user update

userUpdate :: FH ()
userUpdate = runAsUser $ \ clearance _ fsl -> do
    Right (_, user) <- snapRunAction' clearance . queryAction $ LookupUser (fsl ^. fslUserId)
        -- FIXME: handle left
    runPageletForm
               (userUpdateForm
                   (user ^. userName))
               userUpdatePagelet DashboardTabDetails
               $ \ fieldUpdates -> do
        result' <- update $ UpdateUserFields (fsl ^. fslUserId) fieldUpdates clearance
        case result' of
            Right () -> do
                sendFrontendMsg $ FrontendMsgSuccess "User data changed."
                redirect' "/dashboard" 303
            Left e -> do
                crash500 e

emailUpdate :: FH ()
emailUpdate = runAsUser $ \ clearance _ fsl -> do
    runPageletForm emailUpdateForm
                   emailUpdatePagelet DashboardTabDetails
                   $ \ newEmail -> do
        feConfig <- gets (^. frontendCfg)
        result' <- snapRunAction' clearance $
            requestUserEmailChange (fsl ^. fslUserId) newEmail
                (urlConfirm feConfig "/user/update_email_confirm" . fromConfirmationToken)
        case result' of
            Right ()                    -> emailSent
            Left UserEmailAlreadyExists -> emailSent
            Left e                      -> crash500 e
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
            result <- snapRunAction' allowEverything $ confirmUserEmailChange token
            case result of
                Right ()          -> sendFrontendMsg (FrontendMsgSuccess "Change email: success!") >> redirect' "/dashboard" 303
                Left NoSuchToken  -> crash 400 "Change email: no such token."
                Left e            -> crash500 e
        Just (Left _unicodeError) -> crash 400 "Change email: bad token."
        Nothing                   -> crash 400 "Change email: missing token."

passwordUpdate :: FH ()
passwordUpdate = runAsUser $ \ clearance _ fsl -> do
    runPageletForm passwordUpdateForm
                   passwordUpdatePagelet DashboardTabDetails
                   $ \ (oldPw, newPw) -> do
        result' <- snapRunAction' clearance $ changePassword (fsl ^. fslUserId) oldPw newPw
        case result' of
            Right () -> sendFrontendMsg (FrontendMsgSuccess "Change password: success!") >> redirect' "/dashboard" 303
            Left BadCredentials
                     -> sendFrontendMsg (FrontendMsgError "Invalid old password.") >> redirect' "/user/update_password" 303
            Left e   -> crash500 e


-- * services

serviceCreate :: FH ()
serviceCreate = runAsUser $ \ clearance _ fsl -> do
    runPageletForm serviceCreateForm
                   serviceCreatePagelet DashboardTabOwnServices
                   $ \ (name, description) -> do
        result' <- snapRunAction' clearance $ addService (UserA $ fsl ^. fslUserId) name description
        case result' of
            Right (sid, key) -> do
                sendFrontendMsgs
                    [ FrontendMsgSuccess "Added a service!"
                    , FrontendMsgSuccess . cs $ "Service id: " <> show (fromServiceId sid)
                    , FrontendMsgSuccess . cs $ "Service key: " <> show (fromServiceKey key)
                    ]
                redirect' "/dashboard" 303
            Left e -> logger INFO (show e) >> crash 400 "Create service: failed."

-- | (By the time this handler is called, serviceLogin has to have been
-- called so we have a callback to the login page stored in the
-- session state.)
serviceRegister :: FH ()
serviceRegister = runAsUser $ \ clearance _ fsl -> do
    ServiceLoginState sid rr <- getServiceLoginState >>= maybe (crash 400 "Service login: no state.") return

    let present :: ST -> View H.Html -> FH ()
        present formAction view = do
            (_, user)
                <- (snapRunAction' clearance . queryAction $ LookupUser (fsl ^. fslUserId))
                >>= either crash500 return
            (_, service)
                <- (snapRunAction' allowEverything . queryAction $ LookupService sid)
                >>= either (\ e -> crash' 400 e "Service registration: misconfigured service (no service id).") return
            -- FIXME: we are doing a lookup on the service table, but
            -- the service may have an opinion on whether the user is
            -- allowed to look it up.  the user needs to present a
            -- cryptographic proof of the service's ok for lookup
            -- here.
            blaze $ serviceRegisterPage formAction view sid service user

        process :: () -> FH ()
        process () = do
            result <- snapRunAction' allowEverything $ addServiceRegistration (fsl ^. fslToken) sid
            case result of
                Right () -> redirectRR rr
                -- (We match the '()' explicitly here just
                -- because we can, and because nobody has to
                -- wonder what's hidden in the '_'.  No
                -- lazyless counter-magic is involved.)

                Left e -> crash' 400 e "Service registration: error."
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
            eSessionToken :: Either ThentosError SessionToken
                <- snapRunAction' allowEverything $ do  -- FIXME: use allowNothing, fix action to have correct label.
                    addServiceLogin tok sid
                    return tok

            case eSessionToken of
                -- case B: user is logged into thentos and registered
                -- with service.  clean up the 'ServiceLoginState'
                -- stash in thentos session state, extract the
                -- callback URI from the request parameters, inject
                -- the session token we just created, and redirect.
                Right (SessionToken sessionToken) -> do
                    _ <- popServiceLoginState
                    let f = uriQueryL . queryPairsL %~ (("token", cs sessionToken) :)
                    meCallback <- parseURI laxURIParserOptions <$$> getParam "redirect"
                    case meCallback of
                        Just (Right callback) -> redirectURI $ f callback
                        Just (Left _)         -> crash 400 $ "Service login: malformed callback url."
                        Nothing               -> crash 400 $ "Service login: no callback url."

                -- case C: user is logged into thentos, but not
                -- registered with service.  redirect to service
                -- registration page.
                Left NotRegisteredWithService -> do
                    redirect' "/service/register" 303

                -- case D: user is logged into thentos, but something
                -- unexpected went wrong (possibly the session was
                -- corrupted by the user/adversary?).  report error to
                -- log file and user.
                Left e -> do
                    logger INFO $ show e
                    crash 400 "Service login: could not initiate session."

    runAsUserOrNot (\ _ _ -> loggedIn) notLoggedIn


-- | If a service login state exists, consume it, jump back to the
-- service, and log in.  If not, jump to `/dashboard`.
redirectToDashboardOrService :: FH ()
redirectToDashboardOrService = do
    mCallback <- popServiceLoginState
    case mCallback of
        Just (ServiceLoginState _ rr) -> redirectRR rr
        Nothing                       -> redirect' "/dashboard" 303

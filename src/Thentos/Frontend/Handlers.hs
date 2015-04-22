{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE BangPatterns           #-}

module Thentos.Frontend.Handlers where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)
import Crypto.Random (SystemRNG)
import Data.Acid (AcidState)
import Data.ByteString (ByteString)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Functor.Infix ((<$$>))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs)
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import LIO.DCLabel ((/\), (\/))
import Snap.Snaplet.Session (commitSession, setInSession, getFromSession, resetSession)
import Snap.Snaplet.AcidState (getAcidState, update)
import Snap.Blaze (blaze)
import System.Log.Missing (logger)
import Snap.Core (rqURI, getParam, getsRequest, redirect', parseUrlEncoded, printUrlEncoded, modifyResponse, setResponseStatus)
import Snap.Core (getResponse, finishWith, urlEncode, urlDecode)
import Snap.Snaplet (with)
import System.Log (Priority(DEBUG, INFO, WARNING, CRITICAL))
import Text.Digestive.Snap (runForm)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.Text.Lazy as L
import qualified Text.Blaze.Html5 as H

import Thentos.Api
import Thentos.DB
import Thentos.Frontend.Pages
import Thentos.Frontend.Types
import Thentos.Config
import Thentos.Smtp
import Thentos.Types
import Thentos.Util


index :: FH ()
index = blaze indexPage

-- FIXME: for all forms, make sure that on user error, the form is
-- rendered again with response code 409 and a readable error message
-- on top of the form.

-- | FIXME (thanks to Sönke Hahn): this doesn't create a user on
-- errors (e.g.  missing mail address), but does not show an error
-- message.  (Even worse: it returns a 200.  It should response with
-- 409.)
userCreate :: FH ()
userCreate = do
    let clearance = RoleOwnsUnconfirmedUsers *%% RoleOwnsUnconfirmedUsers

    (view, result) <- runForm "create" userCreateForm
    case result of
        Nothing -> blaze $ userCreatePage view
        Just user -> do
            result' <- snapRunAction' clearance $ addUnconfirmedUser user
            case result' of
                Right (_, token) -> do
                    config :: ThentosConfig <- gets (^. cfg)
                    feConfig <- gets (^. frontendCfg)
                    liftIO $ sendUserConfirmationMail
                        (Tagged $ config >>. (Proxy :: Proxy '["smtp"])) user
                        (urlUserCreateConfirm feConfig token)
                    blaze userCreateRequestedPage
                Left UserEmailAlreadyExists -> do
                    config :: ThentosConfig <- gets (^. cfg)
                    liftIO $ sendUserExistsMail (Tagged $ config >>. (Proxy :: Proxy '["smtp"])) (udEmail user)
                    blaze userCreateRequestedPage
                Left e -> logger INFO (show e) >> blaze (errorPage "registration failed.")

urlUserCreateConfirm :: HttpConfig -> ConfirmationToken -> L.Text
urlUserCreateConfirm feConfig (ConfirmationToken token) =
    cs (exposeUrl feConfig) <//> "/user/create_confirm?token="
        <> (L.fromStrict . decodeUtf8 . urlEncode . encodeUtf8 $ token)

userCreateConfirm :: FH ()
userCreateConfirm = do
    let clearance = RoleOwnsUnconfirmedUsers /\ RoleOwnsUsers *%% RoleOwnsUnconfirmedUsers \/ RoleOwnsUsers

    mTokenBS <- getParam "token"
    case ConfirmationToken <$$> (decodeUtf8' <$> mTokenBS) of
        Just (Right token) -> do
            eResult <- snapRunAction' clearance $ confirmNewUser token
            case eResult of
                Right uid -> blaze $ userCreatedPage uid
                Left e@NoSuchPendingUserConfirmation -> do
                    logger INFO (show e)
                    blaze (errorPage "finializing registration failed: unknown token.")
                Left e -> do
                    logger CRITICAL ("unreachable: " ++ show e)
                    blaze (errorPage "finializing registration failed.")
        Just (Left unicodeError) -> do
            logger DEBUG (show unicodeError)
            blaze (errorPage $ show unicodeError)
        Nothing -> do
            logger DEBUG "no token"
            blaze (errorPage "finializing registration failed: token is missing.")

userUpdate :: FH ()
userUpdate = runWithUserClearance $ \ clearance uid -> do
    (userView, result) <- runForm "update" userUpdateForm
    (emailView, _) <- runForm "update_email" emailUpdateForm
    (pwView, _) <- runForm "update_password" passwordUpdateForm
    case result of
        Nothing -> blaze $ userUpdatePage userView emailView pwView
        Just fieldUpdates -> do
            result' <- update $ UpdateUserFields uid fieldUpdates clearance
            case result' of
                Right () -> blaze "User data updated!"
                Left e -> logger INFO (show e) >> blaze (errorPage "user update failed.")

passwordUpdate :: FH ()
passwordUpdate = runWithUserClearance $ \ clearance uid -> do
    (passwordView, result) <- runForm "update_password" passwordUpdateForm
    (userView, _) <- runForm "update" userUpdateForm
    (emailView, _) <- runForm "update_email" emailUpdateForm
    case result of
        Nothing -> blaze $ userUpdatePage userView emailView passwordView
        Just (oldPw, newPw) -> do
            result' <- snapRunAction' clearance $ changePassword uid oldPw newPw
            case result' of
                Right () -> blaze "Password Changed!"
                Left e -> logger INFO (show e) >> blaze (errorPage "user update failed.")

emailUpdate :: FH ()
emailUpdate = runWithUserClearance $ \ clearance uid -> do
    (passwordView, _) <- runForm "update_password" passwordUpdateForm
    (userView, _) <- runForm "update" userUpdateForm
    (emailView, result) <- runForm "update_email" emailUpdateForm
    case result of
        Nothing -> blaze $ userUpdatePage userView emailView passwordView
        Just newEmail -> do
            feConfig <- gets (^. frontendCfg)
            result' <- snapRunAction' clearance $
                requestUserEmailChange uid newEmail
                                       (urlEmailChangeConfirm feConfig)
            case result' of
                Right () -> blaze emailSentPage
                Left UserEmailAlreadyExists -> blaze emailSentPage
                Left e -> logger INFO (show e) >> blaze (errorPage "email update failed.")
  where
    emailSentPage = "Please confirm your new email address through the email we just sent you"

urlEmailChangeConfirm :: HttpConfig -> ConfirmationToken -> L.Text
urlEmailChangeConfirm feConfig (ConfirmationToken token) =
    cs (exposeUrl feConfig) <//> "/user/update_email_confirm?token="
        <> (L.fromStrict . decodeUtf8 . urlEncode . encodeUtf8 $ token)

emailUpdateConfirm :: FH ()
emailUpdateConfirm = do
    mToken <- (>>= urlDecode) <$> getParam "token"
    let meToken = ConfirmationToken <$$> decodeUtf8' <$> mToken
    case meToken of
        Nothing -> blaze $ errorPage "Missing token"
        Just (Left _unicodeError) -> blaze $ errorPage "Bad token"
        Just (Right token) -> do
            result <- snapRunAction' allowEverything $ confirmUserEmailChange token
            case result of
                Right () -> blaze $ "Email address changed"
                Left NoSuchToken -> blaze $ errorPage "No such token"
                Left e -> do
                    logger WARNING (show e)
                    blaze $ errorPage "Error when trying to change email address"

-- | Runs a given handler with the credentials and the id of the currently
-- logged-in user
runWithUserClearance :: (ThentosClearance -> UserId -> FH ()) -> FH ()
runWithUserClearance = runWithUserClearance' ()

runWithUserClearance' :: a -> (ThentosClearance -> UserId -> FH a) -> FH a
runWithUserClearance' def handler = do
    mUid <- fsdUser <$$> getSessionData
    case mUid of
        Nothing -> blaze (errorPage "Not logged in") >> return def
        Just uid -> do
            Right clearance <- snapRunAction' allowEverything $ getUserClearance uid
            handler clearance uid

serviceCreate :: ThentosClearance -> UserId -> FH ()
serviceCreate clearance uid = do
    (view, result) <- runForm "create" serviceCreateForm
    case result of
        Nothing -> blaze $ serviceCreatePage view
        Just (name, description) -> do
            result' <- snapRunAction' clearance $ addService (UserA uid) name description
            case result' of
                Right (sid, key) -> blaze $ serviceCreatedPage sid key
                Left e -> logger INFO (show e) >> blaze (errorPage "service creation failed.")

-- | FIXME[mf] (thanks to Sönke Hahn): The session token seems to be
-- contained in the url. So if people copy the url from the address
-- bar and send it to someone, they will get the same session.  The
-- session token should be in a cookie, shouldn't it?
loginService :: FH ()
loginService = do
    mUid <- fsdUser <$$> getSessionData
    mSid <- ServiceId . cs <$$> getParam "sid"
    case (mUid, mSid) of
        (_, Nothing)         -> blaze "No service id"
        (Nothing, _)         -> blaze $ notLoggedInPage
        (Just uid, Just sid) -> loginSuccess uid sid
  where
    loginSuccess :: UserId -> ServiceId -> FH ()
    loginSuccess uid sid = do
        mCallback <- getParam "redirect"
        case mCallback of
            Nothing -> do
                modifyResponse $ setResponseStatus 400 "Bad Request"
                blaze "400 Bad Request"
                getResponse >>= finishWith
            Just callback -> do
                eSessionToken :: Either ThentosError SessionToken
                    <- snapRunAction' allowEverything $ do  -- FIXME: use allowNothing, fix action to have correct label.
                        tok <- startSessionNoPass (UserA uid)
                        addServiceLogin tok sid
                        return tok
                case eSessionToken of
                    Right sessionToken -> do
                        let (base_url, _query) = BC.break (== '?') callback
                            params             = parseUrlEncoded $ B.drop 1 _query
                            params'            = M.insert "token" [cs $ fromSessionToken sessionToken] params
                            url                = base_url <> "?" <> printUrlEncoded params'
                        redirect' url 303

                            -- FIXME: use Tibell's network-uri package.  (will
                            -- this help in other existing places, too?)

                    Left NotRegisteredWithService -> do
                        error "NotRegisteredWithService"
                    Left e -> do
                        logger INFO (show e) >> blaze (errorPage "could not initiate session.")


loginThentos :: FH ()
loginThentos = do
    (view, formResult) <- runForm "login_thentos" loginThentosForm
    case formResult of
        Just (username, password) -> do
            result <- snapRunAction' allowEverything $
                startThentosSessionByUserName username password
              -- FIXME[mf]: See 'runThentosUpdateWithLabel' in
              -- "Thentos.DB.Core".  Use that to create transaction
              -- 'CheckPasswordWithLabel', then call that with
              -- 'allowNothing' and 'thentosPublic'.
            case result of
                Right (uid, sessionToken) -> with sess $ do
                    let sessionData = FrontendSessionData sessionToken uid
                    setInSession "sessionData" (cs $ Aeson.encode sessionData)
                    commitSession
                    blaze "Logged in"
                Left BadCredentials -> loginFail
                Left _ -> error "logIntoThentosHandler: branch should not be reachable"
                    -- FIXME: this should be handled.  we should
                    -- always allow transactions / actions to throw
                    -- errors.
        Nothing -> blaze $ loginThentosPage view
  where
    loginFail :: FH ()
    loginFail = blaze "Bad username / password combination"


logoutThentos :: FH ()
logoutThentos = blaze logoutThentosPage

loggedOutThentos :: FH ()
loggedOutThentos = with sess $ do
    resetSession
    commitSession
    blaze "Logged out"

checkThentosLogin :: FH ()
checkThentosLogin = do
    mUid <- fsdUser <$$> getSessionData
    case mUid of
        Nothing -> blaze "Not logged in"
        Just uid -> do
            blaze $ "Logged in as user: " <> H.string (show uid)

getSessionData :: FH (Maybe FrontendSessionData)
getSessionData = with sess $ do
    mSessionDataBS <- getFromSession "sessionData"
    return $ case mSessionDataBS of
        Nothing -> Nothing
        Just sessionDataBS -> Aeson.decode $ cs sessionDataBS

resetPasswordRequest :: FH ()
resetPasswordRequest = do
    uri <- getsRequest rqURI
    (view, result) <- runForm (cs uri) resetPasswordRequestForm
    case result of
        Nothing -> blaze $ resetPasswordRequestPage view
        Just address -> do
            config :: ThentosConfig <- gets (^. cfg)
            feConfig <- gets (^. frontendCfg)
            eToken <-
                snapRunAction' allowEverything $ addPasswordResetToken address
            case eToken of
                Left NoSuchUser -> blaze resetPasswordRequestedPage
                Right (user, token) -> do
                    liftIO $ sendPasswordResetMail
                        (Tagged $ config >>. (Proxy :: Proxy '["smtp"])) user
                        (urlPasswordReset feConfig token)
                    blaze resetPasswordRequestedPage
                Left _ -> error "requestPasswordResetHandler: branch should not be reachable"

urlPasswordReset :: HttpConfig -> PasswordResetToken -> L.Text
urlPasswordReset feConfig (PasswordResetToken token) =
    cs (exposeUrl feConfig) <//> "/user/reset_password?token="
        <> (L.fromStrict . decodeUtf8 . urlEncode . encodeUtf8 $ token)

resetPassword :: FH ()
resetPassword = do
    eUrl <- decodeUtf8' <$> getsRequest rqURI
    mToken <- (>>= urlDecode) <$> getParam "token"
    let meToken = PasswordResetToken <$$> decodeUtf8' <$> mToken
    (view, mPassword) <- runForm "password_reset_form" resetPasswordForm
    case (mPassword, meToken, eUrl) of
        -- show reset form
        (Nothing, Just (Right _), Right url) ->
            blaze $ resetPasswordPage url view

        -- process reset form input
        (Just password, Just (Right token), Right _) -> do
            result <- snapRunAction' allowEverything $ Thentos.Api.resetPassword token password
            case result of
                Right () -> blaze $ "Password succesfully changed"
                Left NoSuchToken -> blaze $ errorPage "No such reset token"
                Left e -> do
                    logger WARNING (show e)
                    blaze $ errorPage "Error when trying to change password"

        -- error cases
        (_, _, Left _) -> do
            let msg = "Bad Request: corrupt url!"
            modifyResponse $ setResponseStatus 400 (cs msg)
            blaze (H.text msg)
        (_, Just (Left _), _) -> do
            let msg = "Bad request: bad reset token."
            modifyResponse $ setResponseStatus 400 (cs msg)
            blaze (H.text msg)
        (_, Nothing, _) -> do
            let msg = "Bad request: reset password, but no token."
            modifyResponse $ setResponseStatus 400 (cs msg)
            blaze (H.text msg)


-- * Util

snapRunAction :: (DB -> TimeStamp -> Either ThentosError ThentosClearance) -> Action (MVar SystemRNG) a
      -> FH (Either ThentosError a)
snapRunAction clearanceAbs action = do
    rn :: MVar SystemRNG <- gets (^. rng)
    st :: AcidState DB <- getAcidState
    _cfg :: ThentosConfig <- gets (^. cfg)
    runAction ((st, rn, _cfg), clearanceAbs) action

snapRunAction' :: ThentosClearance -> Action (MVar SystemRNG) a
      -> FH (Either ThentosError a)
snapRunAction' clearance = snapRunAction (\ _ _ -> Right clearance)


-- * Dashboard

dashboardDetails :: FH ()
dashboardDetails = do
    mUid <- fsdUser <$$> getSessionData
    case mUid of
        Nothing -> redirect' "/login_thentos" 303
        Just uid -> do
            eUser  <- snapRunAction' allowEverything . queryAction $ LookupUser uid
            eRoles <- snapRunAction' allowEverything . queryAction $ LookupAgentRoles (UserA uid)
            case (eUser, eRoles) of
                (Right (_, user), Right roles) -> do
                    blaze
                        $ dashboardPagelet roles DashboardTabDetails
                        $ displayUserPagelet user
                _ -> error "unreachable"
                     -- FIXME: error handling.  (we need a better approach for this in general!)

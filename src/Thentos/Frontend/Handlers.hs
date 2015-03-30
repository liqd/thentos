{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE OverloadedStrings      #-}

module Thentos.Frontend.Handlers where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.))
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)
import Crypto.Random (SystemRNG)
import Data.Acid (AcidState)
import Data.ByteString (ByteString)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Functor.Infix ((<$$>))
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs)
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import LIO.DCLabel ((/\), (\/))
import Snap.Snaplet.Session (commitSession, setInSession, getFromSession)
import Snap.Snaplet.AcidState (getAcidState, update)
import Snap.Blaze (blaze)
import System.Log.Missing (logger)
import Snap.Core (rqURI, getParam, getsRequest, redirect', parseUrlEncoded, printUrlEncoded, modifyResponse, setResponseStatus)
import Snap.Core (getResponse, finishWith, urlEncode, urlDecode)
import Snap.Snaplet (Handler, with)
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


index :: Handler FrontendApp FrontendApp ()
index = blaze mainPage

-- FIXME: for all forms, make sure that on use error, the form is
-- rendered again with response code 409 and a readable error message
-- on top of the form.

-- | FIXME (thanks to Sönke Hahn): this doesn't create a user on
-- errors (e.g.  missing mail address), but does not show an error
-- message.  (Even worse: it returns a 200.  It should response with
-- 409.)
userAdd :: Handler FrontendApp FrontendApp ()
userAdd = do
    let clearance = RoleOwnsUnconfirmedUsers *%% RoleOwnsUnconfirmedUsers

    (view, result) <- runForm "create" userForm
    case result of
        Nothing -> blaze $ addUserPage view
        Just user -> do
            result' <- snapRunAction' clearance $ addUnconfirmedUser user
            case result' of
                Right (_, token) -> do
                    config :: ThentosConfig <- gets (^. cfg)
                    feConfig <- gets (^. frontendCfg)
                    liftIO $ sendUserConfirmationMail
                        (Tagged $ config >>. (Proxy :: Proxy '["smtp"])) user
                        (urlSignupConfirm feConfig token)
                    blaze emailSentPage
                Left UserEmailAlreadyExists -> do
                    config :: ThentosConfig <- gets (^. cfg)
                    liftIO $ sendUserExistsMail (Tagged $ config >>. (Proxy :: Proxy '["smtp"])) (udEmail user)
                    blaze emailSentPage
                Left e -> logger INFO (show e) >> blaze (errorPage "registration failed.")

urlSignupConfirm :: HttpConfig -> ConfirmationToken -> L.Text
urlSignupConfirm feConfig (ConfirmationToken token) =
    cs (exposeUrl feConfig) <//> "/user/create_confirm?token="
        <> (L.fromStrict . decodeUtf8 . urlEncode . encodeUtf8 $ token)

userAddConfirm :: Handler FrontendApp FrontendApp ()
userAddConfirm = do
    let clearance = RoleOwnsUnconfirmedUsers /\ RoleOwnsUsers *%% RoleOwnsUnconfirmedUsers \/ RoleOwnsUsers

    mTokenBS <- getParam "token"
    case ConfirmationToken <$$> (decodeUtf8' <$> mTokenBS) of
        Just (Right token) -> do
            eResult <- update $ FinishUserRegistration token clearance
            case eResult of
                Right uid -> blaze $ userAddedPage uid
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

addService :: Handler FrontendApp FrontendApp ()
addService = blaze addServicePage

serviceAdded :: Handler FrontendApp FrontendApp ()
serviceAdded = do
    result <- snapRunAction' allowEverything Thentos.Api.addService
    case result of
        Right (sid, key) -> blaze $ serviceAddedPage sid key
        Left e -> logger INFO (show e) >> blaze (errorPage "could not add service.")

-- | FIXME[mf] (thanks to Sönke Hahn): The session token seems to be
-- contained in the url. So if people copy the url from the address
-- bar and send it to someone, they will get the same session.  The
-- session token should be in a cookie, shouldn't it?
logIntoService :: Handler FrontendApp FrontendApp ()
logIntoService = do
    mUid <- getLoggedInUserId
    mSid <- ServiceId . cs <$$> getParam "sid"
    case (mUid, mSid) of
        (_, Nothing)         -> blaze "No service id"
        (Nothing, _)         -> blaze $ notLoggedInPage
        (Just uid, Just sid) -> loginSuccess uid sid
  where
    loginSuccess :: UserId -> ServiceId -> Handler FrontendApp FrontendApp ()
    loginSuccess uid sid = do
        mCallback <- getParam "redirect"
        case mCallback of
            Nothing -> do
                modifyResponse $ setResponseStatus 400 "Bad Request"
                blaze "400 Bad Request"
                r <- getResponse
                finishWith r
            Just callback -> do
                eSessionToken :: Either ThentosError SessionToken
                    <- snapRunAction' allowEverything $ do
                        tok <- startSessionNoPass (UserA uid)
                        addServiceLogin tok sid
                        return tok
                case eSessionToken of
                    Left e -> logger INFO (show e) >> blaze (errorPage "could not initiate session.")
                    Right sessionToken ->
                        redirect' (redirectUrl callback sessionToken) 303

    redirectUrl :: ByteString -> SessionToken -> ByteString
    redirectUrl serviceProvidedUrl sessionToken =
        let (base_url, _query) = BC.break (== '?') serviceProvidedUrl in
        let params = parseUrlEncoded $ B.drop 1 _query in
        let params' = M.insert "token" [cs $ fromSessionToken sessionToken] params in
        base_url <> "?" <> printUrlEncoded params'


logIntoThentos :: Handler FrontendApp FrontendApp ()
logIntoThentos = do
    (view, result) <- runForm "log_into_thentos" loginForm
    case result of
        Just (username, password) -> do
            emUser <- snapRunAction' allowEverything $ checkPassword username password
              -- FIXME[mf]: See 'runThentosUpdateWithLabel' in
              -- "Thentos.DB.Core".  Use that to create transaction
              -- 'CheckPasswordWithLabel', then call that with
              -- 'allowNothing' and 'thentosPublic'.
            case emUser of
                Right (Just (uid, _)) -> with sess $ do
                    setInSession "user" (cs $ Aeson.encode [uid])
                    commitSession
                    blaze "Logged in"
                Right Nothing -> loginFail
                Left _ -> error "logIntoThentosHandler: branch should not be reachable"
                    -- FIXME: this should be handled.  we should
                    -- always allow transactions / actions to throw
                    -- errors.
        Nothing -> blaze $ logIntoThentosPage view
  where
    loginFail :: Handler FrontendApp FrontendApp ()
    loginFail = blaze "Bad username / password combination"

checkThentosLogin :: Handler FrontendApp FrontendApp ()
checkThentosLogin = do
    mUid <- getLoggedInUserId
    case mUid of
        Nothing -> blaze "Not logged in"
        Just uid -> do
            blaze $ "Logged in as user: " <> H.string (show uid)

getLoggedInUserId :: Handler FrontendApp FrontendApp (Maybe UserId)
getLoggedInUserId = with sess $ do
    mUserText <- getFromSession "user"
    return $ case mUserText of
        Nothing -> Nothing
        Just userText ->
            let mlUid = Aeson.decode . cs $ userText :: Maybe [UserId] in
            join $ listToMaybe <$> mlUid

requestPasswordReset :: Handler FrontendApp FrontendApp ()
requestPasswordReset = do
    uri <- getsRequest rqURI
    (view, result) <- runForm (cs uri) requestPasswordResetForm
    case result of
        Nothing -> blaze $ requestPasswordResetPage view
        Just address -> do
            config :: ThentosConfig <- gets (^. cfg)
            feConfig <- gets (^. frontendCfg)
            eToken <-
                snapRunAction' allowEverything $ addPasswordResetToken address
            case eToken of
                Left NoSuchUser -> blaze emailSentPage
                Right (user, token) -> do
                    liftIO $ sendPasswordResetMail
                        (Tagged $ config >>. (Proxy :: Proxy '["smtp"])) user
                        (urlResetPassword feConfig token)
                    blaze emailSentPage
                Left _ -> error "requestPasswordResetHandler: branch should not be reachable"

urlResetPassword :: HttpConfig -> PasswordResetToken -> L.Text
urlResetPassword feConfig (PasswordResetToken token) =
    cs (exposeUrl feConfig) <//> "/user/reset_password?token="
        <> (L.fromStrict . decodeUtf8 . urlEncode . encodeUtf8 $ token)

resetPassword :: Handler FrontendApp FrontendApp ()
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
                Left NoSuchResetToken -> blaze $ errorPage "No such reset token"
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
      -> Handler FrontendApp FrontendApp (Either ThentosError a)
snapRunAction clearanceAbs action = do
    rn :: MVar SystemRNG <- gets (^. rng)
    st :: AcidState DB <- getAcidState
    _cfg :: ThentosConfig <- gets (^. cfg)
    runAction ((st, rn, _cfg), clearanceAbs) action

snapRunAction' :: ThentosClearance -> Action (MVar SystemRNG) a
      -> Handler FrontendApp FrontendApp (Either ThentosError a)
snapRunAction' clearance = snapRunAction (\ _ _ -> Right clearance)

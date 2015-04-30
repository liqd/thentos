{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Thentos.Frontend.Handlers where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.), (.~), (%~))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)
import Crypto.Random (SystemRNG)
import Data.Acid (AcidState)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Functor.Infix ((<$$>))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (SBS, cs)
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import LIO.DCLabel ((/\), (\/))
import Snap.Blaze (blaze)
import Snap.Core (getResponse, finishWith, urlEncode, urlDecode)
import Snap.Core (rqURI, getParam, getsRequest, redirect', modifyResponse, setResponseStatus)
import Snap.Snaplet.AcidState (getAcidState, update)
import Snap.Snaplet.Session (commitSession, setInSession, getFromSession, resetSession)
import Snap.Snaplet (with)
import System.Log.Missing (logger)
import System.Log (Priority(DEBUG, INFO, WARNING, CRITICAL))
import Text.Digestive.Snap (runForm)
import URI.ByteString (URI(..), RelativeRef(..), URIParserOptions)
import URI.ByteString (parseURI, parseRelativeRef, laxURIParserOptions, serializeURI, serializeRelativeRef)
import URI.ByteString (rrPathL, uriQueryL, queryPairsL)

import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy as L
import qualified Text.Blaze.Html5 as H

import Thentos.Api
import Thentos.Config
import Thentos.DB
import Thentos.Frontend.Pages
import Thentos.Frontend.Types
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
                Left e -> logger INFO (show e) >> crash 400 "registration failed."

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
                    crash 400 "finalizing registration failed: unknown token."
                Left e -> do
                    logger CRITICAL ("unreachable: " ++ show e)
                    crash 400 "finializing registration failed."
        Just (Left unicodeError) -> do
            crash' 400 unicodeError "bad user confirmation link."
        Nothing -> do
            crash 400 "no user confirmation token."

userUpdate :: FH ()
userUpdate = runAsUser $ \ clearance session -> do
    (userView, result) <- runForm "update" userUpdateForm
    (emailView, _) <- runForm "update_email" emailUpdateForm
    (pwView, _) <- runForm "update_password" passwordUpdateForm
    case result of
        Nothing -> blaze $ userUpdatePage userView emailView pwView
        Just fieldUpdates -> do
            result' <- update $ UpdateUserFields (fsdUser session) fieldUpdates clearance
            case result' of
                Right () -> blaze "User data updated!"
                Left e -> logger INFO (show e) >> crash 400 "user update failed."

passwordUpdate :: FH ()
passwordUpdate = runAsUser $ \ clearance session -> do
    (passwordView, result) <- runForm "update_password" passwordUpdateForm
    (userView, _) <- runForm "update" userUpdateForm
    (emailView, _) <- runForm "update_email" emailUpdateForm
    case result of
        Nothing -> blaze $ userUpdatePage userView emailView passwordView
        Just (oldPw, newPw) -> do
            result' <- snapRunAction' clearance $ changePassword (fsdUser session) oldPw newPw
            case result' of
                Right () -> blaze "Password Changed!"
                Left e -> logger INFO (show e) >> crash 400 "user update failed."

emailUpdate :: FH ()
emailUpdate = runAsUser $ \ clearance session -> do
    (passwordView, _) <- runForm "update_password" passwordUpdateForm
    (userView, _) <- runForm "update" userUpdateForm
    (emailView, result) <- runForm "update_email" emailUpdateForm
    case result of
        Nothing -> blaze $ userUpdatePage userView emailView passwordView
        Just newEmail -> do
            feConfig <- gets (^. frontendCfg)
            result' <- snapRunAction' clearance $
                requestUserEmailChange (fsdUser session) newEmail
                                       (urlEmailChangeConfirm feConfig)
            case result' of
                Right () -> blaze emailSentPage
                Left UserEmailAlreadyExists -> blaze emailSentPage
                Left e -> logger INFO (show e) >> crash 400 "email update failed."
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
        Nothing -> crash 400 "change email: missing token."
        Just (Left _unicodeError) -> crash 400 "change email: bad token."
        Just (Right token) -> do
            result <- snapRunAction' allowEverything $ confirmUserEmailChange token
            case result of
                Right () -> blaze $ "change email: success!"
                Left NoSuchToken -> crash 400 "change email: no such token."
                Left e -> do
                    logger WARNING (show e)
                    crash 400 "change email: error."

-- | Runs a given handler with the credentials and the session data of the
-- currently logged-in user
runAsUser :: (ThentosClearance -> FrontendSessionData -> FH a) -> FH a
runAsUser handler = do
    mSessionData <- getSessionData
    case mSessionData of
        Nothing -> redirect' "/login_thentos" 303
        Just sessionData -> do
            Right clearance <- snapRunAction' allowEverything $ getUserClearance (fsdUser sessionData)
            handler clearance sessionData
 where
    getSessionData :: FH (Maybe FrontendSessionData)
    getSessionData = with sess $ do
        mSessionDataBS <- getFromSession "sessionData"
        return $ mSessionDataBS >>= Aeson.decode . cs

serviceCreate :: FH ()
serviceCreate = runAsUser $ \clearance session -> do
    (view, result) <- runForm "create" serviceCreateForm
    case result of
        Nothing -> blaze $ serviceCreatePage view
        Just (name, description) -> do
            result' <- snapRunAction' clearance $ addService (UserA $ fsdUser session) name description
            case result' of
                Right (sid, key) -> blaze $ serviceCreatedPage sid key
                Left e -> logger INFO (show e) >> crash 400 "service creation failed."


-- | construct the state from context, writes it to snap session, and
-- return it.
serviceRegisterWriteState :: FH ServiceRegisterState
serviceRegisterWriteState = do
    mSid :: Maybe ServiceId <- ServiceId . cs <$$> getParam "sid"
    state :: ServiceRegisterState <- do
        uriBS :: SBS <- getsRequest rqURI
        case (parseRelativeRef laxURIParserOptions uriBS, mSid) of
            (Right rr, Just sid) -> return $ ServiceRegisterState (rrPathL .~ "/service/login" $ rr, sid)
            bad -> crash 500 $ "bad request uri: " <> cs (show bad)
    updateSessionData (\ fsd -> fsd { fsdServiceRegisterState = Just state })
    return state

-- | recover state from snap session.
serviceRegisterReadState :: FrontendSessionData -> FH ServiceRegisterState
serviceRegisterReadState session = do
    case fsdServiceRegisterState session of
        Just state -> return state
        Nothing    -> crash 500 "service registration: no info in session state."

serviceRegister :: FH ()
serviceRegister = runAsUser $ \ clearance session -> do
    (view, result) <- runForm "register" serviceRegisterForm
    case result of
        Nothing -> do
            ServiceRegisterState (_, sid) <- serviceRegisterWriteState
            Right (_, user)    <- snapRunAction' clearance . queryAction $ LookupUser (fsdUser session)
            Right (_, service) <- snapRunAction' allowEverything . queryAction $ LookupService sid
            -- FIXME: service needs to prove its ok with user looking it up here.
            -- FIXME: handle 'Left's
                -- specifically, service might not exist
            blaze $ serviceRegisterPage view sid service user

        Just () -> do
            ServiceRegisterState (rr, sid) <- serviceRegisterReadState session
            Right _ <- snapRunAction' allowEverything $ addServiceRegistration (fsdToken session) sid
            redirect' (cs . toLazyByteString . serializeRelativeRef $ rr) 303


-- | FIXME[mf] (thanks to Sönke Hahn): The session token seems to be
-- contained in the url. So if people copy the url from the address
-- bar and send it to someone, they will get the same session.  The
-- session token should be in a cookie, shouldn't it?
loginService :: FH ()
loginService = runAsUser $ \ _ session -> do
    mSid <- ServiceId . cs <$$> getParam "sid"
    case mSid of
        Nothing  -> crash 400 "No service id"
        Just sid -> loginSuccess session sid
  where
    loginSuccess :: FrontendSessionData -> ServiceId -> FH ()
    loginSuccess session sid = do
        mCallback <- getParam "redirect"
        case mCallback of
            Just callback -> do
                let tok = fsdToken session
                eSessionToken :: Either ThentosError SessionToken
                    <- snapRunAction' allowEverything $ do  -- FIXME: use allowNothing, fix action to have correct label.
                        addServiceLogin tok sid
                        return tok
                case eSessionToken of
                    Right sessionToken -> do
                        let f = uriQueryL . queryPairsL %~ (("token", cs $ fromSessionToken sessionToken) :)
                        tweakURI f callback >>= (`redirect'` 303)

                    Left NotRegisteredWithService -> do
                        let f = rrPathL .~ "/service/register"
                        tweakRelativeRqRef f >>= (`redirect'` 303)

                    Left e -> do
                        logger INFO $ show e
                        crash 400 "could not initiate session."

            bad -> crash' 400 bad "bad request."


tweakRelativeRqRef :: (RelativeRef -> RelativeRef) -> FH SBS
tweakRelativeRqRef tweak = getsRequest rqURI >>= tweakRelativeRef tweak

tweakRelativeRef :: (RelativeRef -> RelativeRef) -> SBS -> FH SBS
tweakRelativeRef = _tweakURI parseRelativeRef serializeRelativeRef

tweakURI :: (URI -> URI) -> SBS -> FH SBS
tweakURI = _tweakURI parseURI serializeURI

_tweakURI :: forall e t t' . (Show e) =>
                   (URIParserOptions -> SBS -> Either e t)
                -> (t' -> Builder)
                -> (t -> t')
                -> SBS
                -> FH SBS
_tweakURI parse serialize tweak uriBS = do
    let ok uri = do
            return (cs . toLazyByteString . serialize . tweak $ uri)
        er err = do
            logger CRITICAL $ show (err, uriBS)
            crash 500 ("bad request uri: " <> cs (show uriBS))
    either er ok $ parse laxURIParserOptions uriBS


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
                    let sessionData = FrontendSessionData sessionToken uid Nothing
                    setInSession "sessionData" (cs $ Aeson.encode sessionData)
                    commitSession
                    blaze "Logged in"  -- FIXME: redirect to dashboard; show logged-in status in dashboard.
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
logoutThentos = runAsUser $ \ _ sessionData -> do
    eServiceNames <- snapRunAction' allowEverything $ getSessionServiceNames (fsdToken sessionData) (fsdUser sessionData)
    case eServiceNames of
        Right serviceNames -> blaze $ logoutThentosPage serviceNames
        Left NoSuchUser -> crash 400 "User does not exist"
        Left NoSuchSession -> crash 400 "Session does not exist"
        Left _ -> error "unreachable" -- FIXME

loggedOutThentos :: FH ()
loggedOutThentos = runAsUser $ \ _ session -> do
    _ <- snapRunAction' allowEverything . updateAction $ EndSession (fsdToken session)
    with sess $ do
        resetSession
        commitSession
        blaze "Logged out"

-- | This is probably not race-condition-free.
updateSessionData :: (FrontendSessionData -> FrontendSessionData) -> FH ()
updateSessionData op = with sess $ do
    mSessionData <- (>>= Aeson.decode . cs) <$> getFromSession "sessionData"
    case mSessionData of
        Just sessionData -> do
            setInSession "sessionData" . cs . Aeson.encode . op $ sessionData
            commitSession
        Nothing -> return ()

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
                Left NoSuchToken -> crash 400 "no such reset token."
                Left e -> do
                    logger WARNING (show e)
                    crash 400 "chnage password: error"

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

crash' :: (Show a) => Int -> a -> SBS -> FH b
crash' status logMsg usrMsg = do
    logger DEBUG $ show (status, logMsg, usrMsg)
    modifyResponse $ setResponseStatus status usrMsg
    blaze . errorPage . cs $ usrMsg
    getResponse >>= finishWith

crash :: Int -> SBS -> FH b
crash status usrMsg = crash' status () usrMsg

snapRunAction :: (DB -> Timestamp -> Either ThentosError ThentosClearance) -> Action (MVar SystemRNG) a
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
dashboardDetails = runAsUser $ \ _ session -> do
    let uid = fsdUser session
    eUser  <- snapRunAction' allowEverything . queryAction $ LookupUser uid
    eRoles <- snapRunAction' allowEverything . queryAction $ LookupAgentRoles (UserA uid)
    case (eUser, eRoles) of
        (Right (_, user), Right roles) -> do
            blaze
                $ dashboardPagelet roles DashboardTabDetails
                $ displayUserPagelet user
        _ -> error "unreachable"
                 -- FIXME: error handling.  (we need a better approach for this in general!)

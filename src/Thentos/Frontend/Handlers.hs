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
import Data.ByteString.Builder (toLazyByteString)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Functor.Infix ((<$$>))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (SBS, ST, cs)
import Data.Text.Encoding (decodeUtf8')
import LIO.DCLabel ((/\), (\/))
import Snap.Blaze (blaze)
import Snap.Core (Method(GET, POST), method)
import Snap.Core (rqURI, getParam, getsRequest, redirect', modifyResponse, setResponseStatus)
import Snap.Core (urlDecode)
import Snap.Snaplet.AcidState (update)
import Snap.Snaplet.Session (commitSession, setInSession, resetSession)
import Snap.Snaplet (with)
import System.Log.Missing (logger)
import System.Log (Priority(DEBUG, INFO, WARNING, CRITICAL))
import Text.Digestive.Snap (runForm)
import URI.ByteString (parseRelativeRef, laxURIParserOptions, serializeRelativeRef)
import URI.ByteString (RelativeRef(..), Query(..))
import URI.ByteString (rrPathL, uriQueryL, queryPairsL)

import qualified Data.Aeson as Aeson
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
            eResult <- snapRunAction' clearance $ confirmNewUser token
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
                    redirect' "/dashboard" 303
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
        userLoginCallAction $ startThentosSessionByUserName username password
        sendFrontendMsg $ FrontendMsgSuccess "Login successful.  Welcome to Thentos!"
        redirect' "/dashboard" 303


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
        Right (uid, sessionToken) -> with sess $ do
            let sessionData = FrontendSessionData sessionToken uid Nothing []
            setInSession "sessionData" (cs $ Aeson.encode sessionData)
            commitSession
            redirect' "/dashboard" 303
        Left BadCredentials -> redirectRR
            (RelativeRef Nothing "/user/login" (Query [("error_msg", "Bad username or password.")]) Nothing)
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
userLogoutConfirm = runAsUser $ \ _ sessionData -> do
    eServiceNames <- snapRunAction' allowEverything $ getSessionServiceNames (fsdToken sessionData) (fsdUser sessionData)
    case eServiceNames of
        Right serviceNames -> renderDashboard DashboardTabLogout (userLogoutConfirmPagelet "/user/logout" serviceNames)
        Left NoSuchUser -> crash 500 "User does not exist."
        Left NoSuchSession -> crash 500 "Session does not exist."
        Left _ -> error "unreachable" -- FIXME

userLogoutDone :: FH ()
userLogoutDone = runAsUser $ \ _ session -> do
    _ <- snapRunAction' allowEverything . updateAction $ EndSession (fsdToken session)
    with sess $ do
        resetSession
        commitSession
        blaze userLogoutDonePage


-- * user update

userUpdate :: FH ()
userUpdate = runAsUser $ \ clearance session -> do
    Right (_, user) <- snapRunAction' clearance . queryAction $ LookupUser (fsdUser session)
        -- FIXME: handle left
    runPageletForm
               (userUpdateForm
                   (user ^. userName))
               userUpdatePagelet DashboardTabDetails
               $ \ fieldUpdates -> do
        result' <- update $ UpdateUserFields (fsdUser session) fieldUpdates clearance
        case result' of
            Right () -> do
                sendFrontendMsg $ FrontendMsgSuccess "User data changed."
                redirect' "/dashboard" 303
            Left e -> do
                logger INFO (show e)
                crash 500 "User update failed."

emailUpdate :: FH ()
emailUpdate = runAsUser $ \ clearance session -> do
    runPageletForm emailUpdateForm
                   emailUpdatePagelet DashboardTabDetails
                   $ \ newEmail -> do
        feConfig <- gets (^. frontendCfg)
        result' <- snapRunAction' clearance $
            requestUserEmailChange (fsdUser session) newEmail
                (urlConfirm feConfig "/user/update_email_confirm" . fromConfirmationToken)
        case result' of
            Right ()                    -> emailSent
            Left UserEmailAlreadyExists -> emailSent
            Left e                      -> logger INFO (show e) >> crash 500 "Change email: error."
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
                Left NoSuchToken  ->                         crash 400 "Change email: no such token."
                Left e            -> logger INFO (show e) >> crash 500 "Change email: error."
        Just (Left _unicodeError) ->                         crash 400 "Change email: bad token."
        Nothing                   ->                         crash 400 "Change email: missing token."

passwordUpdate :: FH ()
passwordUpdate = runAsUser $ \ clearance session -> do
    runPageletForm passwordUpdateForm
                   passwordUpdatePagelet DashboardTabDetails
                   $ \ (oldPw, newPw) -> do
        result' <- snapRunAction' clearance $ changePassword (fsdUser session) oldPw newPw
        case result' of
            Right () -> sendFrontendMsg (FrontendMsgSuccess "Change password: success!") >> redirect' "/dashboard" 303
            Left BadCredentials
                     -> sendFrontendMsg (FrontendMsgError "Invalid old password.") >> redirect' "/user/update_password" 303
            Left e   -> logger INFO (show e) >> crash 500 "Change password: error."


-- * services

serviceCreate :: FH ()
serviceCreate = runAsUser $ \ clearance session -> do
    runPageletForm serviceCreateForm
                   serviceCreatePagelet DashboardTabOwnServices
                   $ \ (name, description) -> do
        result' <- snapRunAction' clearance $ addService (UserA $ fsdUser session) name description
        case result' of
            Right (sid, key) -> do
                sendFrontendMsgs
                    [ FrontendMsgSuccess "Added a service!"
                    , FrontendMsgSuccess . cs $ "Service id: " <> show (fromServiceId sid)
                    , FrontendMsgSuccess . cs $ "Service key: " <> show (fromServiceKey key)
                    ]
                redirect' "/dashboard" 303
            Left e -> logger INFO (show e) >> crash 400 "Create service: failed."


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
    updateSessionData (\ fsd -> (fsd { fsdServiceRegisterState = Just state }, ()))
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
            blaze $ serviceRegisterPage "register" view sid service user

        Just () -> do
            ServiceRegisterState (rr, sid) <- serviceRegisterReadState session
            Right _ <- snapRunAction' allowEverything $ addServiceRegistration (fsdToken session) sid
            redirect' (cs . toLazyByteString . serializeRelativeRef $ rr) 303


-- | Coming from a service site, handle the authentication and
-- redirect to service with valid session token.  This may happen in a
-- series of redirects through the thenots frontend; the state of this
-- series is stored in `fsdServiceRegisterState`.
--
-- FIXME[mf] (thanks to Sönke Hahn): The session token seems to be
-- contained in the url. So if people copy the url from the address
-- bar and send it to someone, they will get the same session.  The
-- session token should be in a cookie, shouldn't it?
serviceLogin :: FH ()
serviceLogin = runAsUser $ \ _ session -> do
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

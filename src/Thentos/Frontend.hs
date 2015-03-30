{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module Thentos.Frontend where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.MVar (MVar)
import Control.Lens (makeLenses, view, (^.))
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
import Snap.Blaze (blaze)
import Snap.Core (getResponse, finishWith, method, Method(GET, POST), ifTop, urlEncode, urlDecode)
import Snap.Core (rqURI, getParam, getsRequest, redirect', parseUrlEncoded, printUrlEncoded, modifyResponse, setResponseStatus)
import Snap.Http.Server (defaultConfig, setBind, setPort)
import Snap.Snaplet.AcidState (Acid, acidInitManual, HasAcid(getAcidStore), getAcidState, update)
import Snap.Snaplet.Session.Backends.CookieSession (initCookieSessionManager)
import Snap.Snaplet.Session.SessionManager (SessionManager)
import Snap.Snaplet.Session (commitSession, setInSession, getFromSession)
import Snap.Snaplet (Snaplet, SnapletInit, snapletValue, makeSnaplet, nestSnaplet, addRoutes, Handler, with)
import System.Log (Priority(DEBUG, INFO, WARNING, CRITICAL))
import Text.Digestive.Snap (runForm)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.Text.Lazy as L
import qualified Text.Blaze.Html5 as H

import System.Log.Missing (logger)
import Thentos.Api
import Thentos.Config
import Thentos.DB
import Thentos.Frontend.Pages
import Thentos.Frontend.Util (serveSnaplet)
import Thentos.Smtp
import Thentos.Types
import Thentos.Util ((<//>))


data FrontendApp =
    FrontendApp
      { _db :: Snaplet (Acid DB)
      , _rng :: MVar SystemRNG
      , _cfg :: ThentosConfig
      , _sess :: Snaplet SessionManager
      }

makeLenses ''FrontendApp

instance HasAcid FrontendApp DB where
    getAcidStore = view (db . snapletValue)

runFrontend :: HttpConfig -> ActionStateGlobal (MVar SystemRNG) -> IO ()
runFrontend config asg = do
    logger INFO $ "running frontend on " <> show (bindUrl config) <> "."
    serveSnaplet (setBind host $ setPort port defaultConfig) (frontendApp asg)
  where
    host :: ByteString = cs $ config >>. (Proxy :: Proxy '["bind_host"])
    port :: Int = config >>. (Proxy :: Proxy '["bind_port"])

frontendApp :: ActionStateGlobal (MVar SystemRNG) -> SnapletInit FrontendApp FrontendApp
frontendApp (st, rn, _cfg) = makeSnaplet "Thentos" "The Thentos universal user management system" Nothing $ do
    addRoutes routes
    FrontendApp <$>
        (nestSnaplet "acid" db $ acidInitManual st) <*>
        (return rn) <*>
        (return _cfg) <*>
        (nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600))

routes :: [(ByteString, Handler FrontendApp FrontendApp ())]
routes = [ ("", ifTop $ mainPageHandler)

         , ("log_into_thentos", logIntoThentosHandler)

         -- FIXME: make "user/" a sub-routing-table.
         , ("user_create", userAddHandler)
         , ("user_create_confirm", userAddConfirmHandler)
         , ("user_reset_password_request", requestPasswordResetHandler)
         , ("user_reset_password", resetPasswordHandler)
         , ("user_update", ?)

         , ("service_create", method GET addServiceHandler)
         , ("service_create", method POST addServiceHandler)

         , ("log_into_service", logIntoServiceHandler)
         , ("check_thentos_login", checkThentosLoginHandler)
         ]

-- FIXME: move all handlers to separate module; rename "blaHandler" to
-- "bla".  import qualified as H.

mainPageHandler :: Handler FrontendApp FrontendApp ()
mainPageHandler = blaze mainPage

-- FIXME: for all forms, make sure that on use error, the form is
-- rendered again with response code 409 and a readable error message
-- on top of the form.

-- | FIXME (thanks to Sönke Hahn): this doesn't create a user on
-- errors (e.g.  missing mail address), but does not show an error
-- message.  (Even worse: it returns a 200.  It should response with
-- 409.)
userAddHandler :: Handler FrontendApp FrontendApp ()
userAddHandler = do
    let clearance = RoleOwnsUnconfirmedUsers *%% RoleOwnsUnconfirmedUsers

    (_view, result) <- runForm "create_user" userForm
    case result of
        Nothing -> blaze $ addUserPage _view
        Just user -> do
            result' <- snapRunAction' clearance $ addUnconfirmedUser user
            case result' of
                Right (_, token) -> do
                    config :: ThentosConfig <- gets (^. cfg)
                    let Just (feConfig :: HttpConfig) = Tagged <$> config >>. (Proxy :: Proxy '["frontend"])
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
    cs (exposeUrl feConfig) <//> "/signup_confirm?token="
        <> (L.fromStrict . decodeUtf8 . urlEncode . encodeUtf8 $ token)

userAddConfirmHandler :: Handler FrontendApp FrontendApp ()
userAddConfirmHandler = do
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

addServiceHandler :: Handler FrontendApp FrontendApp ()
addServiceHandler = blaze addServicePage

serviceAddedHandler :: Handler FrontendApp FrontendApp ()
serviceAddedHandler = do
    result <- snapRunAction' allowEverything addService
    case result of
        Right (sid, key) -> blaze $ serviceAddedPage sid key
        Left e -> logger INFO (show e) >> blaze (errorPage "could not add service.")

-- | FIXME[mf] (thanks to Sönke Hahn): The session token seems to be
-- contained in the url. So if people copy the url from the address
-- bar and send it to someone, they will get the same session.  The
-- session token should be in a cookie, shouldn't it?
logIntoServiceHandler :: Handler FrontendApp FrontendApp ()
logIntoServiceHandler = do
    mUid <- getLoggedInUserId
    mSid <- ServiceId . cs <$$> getParam "sid"
    case (mUid, mSid) of
        (_, Nothing)         -> blaze "No service id"
        (Nothing, _)         -> blaze $ notLoggedInPage "localhost:7002"  -- FIXME: use url from cfg.
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


logIntoThentosHandler :: Handler FrontendApp FrontendApp ()
logIntoThentosHandler = do
    (_view, result) <- runForm "log_into_thentos" loginForm
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
        Nothing -> blaze $ logIntoThentosPage _view
  where
    loginFail :: Handler FrontendApp FrontendApp ()
    loginFail = blaze "Bad username / password combination"

checkThentosLoginHandler :: Handler FrontendApp FrontendApp ()
checkThentosLoginHandler = do
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

requestPasswordResetHandler :: Handler FrontendApp FrontendApp ()
requestPasswordResetHandler = do
    uri <- getsRequest rqURI
    (_view, result) <- runForm (cs uri) requestPasswordResetForm
    case result of
        Nothing -> blaze $ requestPasswordResetPage _view
        Just address -> do
            config :: ThentosConfig <- gets (^. cfg)
            let Just (feConfig :: HttpConfig) = Tagged <$> config >>. (Proxy :: Proxy '["frontend"])
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
    cs (exposeUrl feConfig) <//> "/reset_password?token="
        <> (L.fromStrict . decodeUtf8 . urlEncode . encodeUtf8 $ token)

resetPasswordHandler :: Handler FrontendApp FrontendApp ()
resetPasswordHandler = do
    eUrl <- decodeUtf8' <$> getsRequest rqURI
    mToken <- (>>= urlDecode) <$> getParam "token"
    let meToken = PasswordResetToken <$$> decodeUtf8' <$> mToken
    (_view, mPassword) <- runForm "password_reset_form" resetPasswordForm
    case (mPassword, meToken, eUrl) of
        -- show reset form
        (Nothing, Just (Right _), Right url) -> do
            blaze $ resetPasswordPage url _view

        -- process reset form input
        (Just password, Just (Right token), Right _) -> do
            result <- snapRunAction' allowEverything $ resetPassword token password
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

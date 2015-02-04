{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TemplateHaskell                          #-}

module Frontend (runFrontend) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.MVar (MVar)
import Control.Lens (makeLenses, view, (^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)
import Control.Monad (when)
import Crypto.Random (SystemRNG)
import Data.Acid (AcidState)
import Data.ByteString (ByteString)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Thyme (getCurrentTime)
import Data.Thyme.Time ()  -- (instance Num NominalDiffTime)
import Snap.Blaze (blaze)
import Snap.Core (getResponse, finishWith, method, Method(GET, POST))
import Snap.Core (rqURI, getParam, getsRequest, redirect', parseUrlEncoded, printUrlEncoded, modifyResponse, setResponseStatus)
import Snap.Http.Server (defaultConfig, setBind, setPort)
import Snap.Snaplet.AcidState (Acid, acidInitManual, HasAcid(getAcidStore), getAcidState, update, query)
import Snap.Snaplet (Snaplet, SnapletInit, snapletValue, makeSnaplet, nestSnaplet, addRoutes, Handler)
import Text.Digestive.Snap (runForm)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M

import Api
import Config
import DB
import Types

import Frontend.Pages (addUserPage, userForm, userAddedPage, loginForm, loginPage, errorPage, addServicePage, serviceAddedPage)
import Frontend.Util (serveSnaplet)
import Frontend.Mail (sendUserConfirmationMail)


data FrontendApp =
    FrontendApp
      { _db :: Snaplet (Acid DB)
      , _rng :: MVar SystemRNG
      , _cfg :: ThentosConfig
      }

makeLenses ''FrontendApp

instance HasAcid FrontendApp DB where
    getAcidStore = view (db . snapletValue)

runFrontend :: ByteString -> Int -> ActionStateGlobal (MVar SystemRNG) -> IO ()
runFrontend host port asg = serveSnaplet (setBind host $ setPort port defaultConfig) (frontendApp asg)

frontendApp :: ActionStateGlobal (MVar SystemRNG) -> SnapletInit FrontendApp FrontendApp
frontendApp (st, rn, _cfg) = makeSnaplet "Thentos" "The Thentos universal user management system" Nothing $ do
    addRoutes routes
    FrontendApp <$>
        (nestSnaplet "acid" db $ acidInitManual st) <*>
        (return rn) <*>
        (return _cfg)

routes :: [(ByteString, Handler FrontendApp FrontendApp ())]
routes = [ ("login", loginHandler)
         , ("create_user", userAddHandler)
         , ("signup_confirm", userAddConfirmHandler)
         , ("create_service", method GET addServiceHandler)
         , ("create_service", method POST serviceAddedHandler)
         ]

userAddHandler :: Handler FrontendApp FrontendApp ()
userAddHandler = do
    (_view, result) <- runForm "create_user" userForm
    case result of
        Nothing -> blaze $ addUserPage _view
        Just user -> do
            result' <- snapRunAction' allowEverything $ addUnconfirmedUser user
            case result' of
                Right (ConfirmationToken token) -> do
                    -- FIXME: callback base url must be configurable
                    let url = "http://localhost:8002/signup_confirm?token=" <> encodeUtf8 token
                    liftIO $ sendUserConfirmationMail user url
                    blaze "Please check your email!"
                Left e -> blaze . errorPage $ show e

userAddConfirmHandler :: Handler FrontendApp FrontendApp ()
userAddConfirmHandler = do
    Just tokenBS <- getParam "token" -- FIXME: error handling
    case ConfirmationToken <$> decodeUtf8' tokenBS of
        Right token -> do
            result <- update $ FinishUserRegistration token allowEverything
            case result of
                Right uid -> blaze $ userAddedPage uid
                Left e -> blaze . errorPage $ show e
        Left unicodeError -> blaze . errorPage $ show unicodeError

addServiceHandler :: Handler FrontendApp FrontendApp ()
addServiceHandler = blaze addServicePage

serviceAddedHandler :: Handler FrontendApp FrontendApp ()
serviceAddedHandler = do
    result <- snapRunAction' allowEverything addService
    case result of
        Right (sid, key) -> blaze $ serviceAddedPage sid key
        Left e -> blaze . errorPage $ show e

loginHandler :: Handler FrontendApp FrontendApp ()
loginHandler = do
    uri <- getsRequest rqURI
    (_view, result) <- runForm (cs uri) loginForm
    case result of
        Nothing -> blaze $ loginPage _view uri
        Just (name, password) -> do
            eUser <- query $ LookupUserByName name allowEverything
            case eUser of
                Right (uid, user) ->
                    if password == (user ^. userPassword)
                        then loginSuccess uid
                        else loginFail
                Left NoSuchUser -> loginFail
                Left e -> blaze . errorPage $ show e
  where
    loginFail :: Handler FrontendApp FrontendApp ()
    loginFail = blaze "Bad username / password combination"

    loginSuccess :: UserId -> Handler FrontendApp FrontendApp ()
    loginSuccess uid = do
        now <- liftIO getCurrentTime
        mSid <- getParam "sid"
        mCallback <- getParam "redirect"
        when (isNothing mSid || isNothing mCallback) $ do
            modifyResponse $ setResponseStatus 400 "Bad Request"
            blaze "400 Bad Request"
            r <- getResponse
            finishWith r
        let (Just sid, Just callback) = (mSid, mCallback)
        -- FIXME: how long should the session live?
        eSessionToken :: Either DbError SessionToken
            <- snapRunAction' allowEverything $
                startSession uid (ServiceId $ cs sid) (TimeStamp now) (Timeout $ 14 * 24 * 3600)
        case eSessionToken of
            Left e -> blaze . errorPage $ show e
            Right sessionToken ->
                redirect' (redirectUrl callback sessionToken) 303

    redirectUrl :: ByteString -> SessionToken -> ByteString
    redirectUrl serviceProvidedUrl sessionToken =
        let (base_url, _query) = BC.break (== '?') serviceProvidedUrl in
        let params = parseUrlEncoded $ B.drop 1 _query in
        let params' = M.insert "token" [cs $ fromSessionToken sessionToken] params in
        base_url <> "?" <> printUrlEncoded params'


snapRunAction :: (DB -> Either DbError ThentosClearance) -> Action (MVar SystemRNG) a
      -> Handler FrontendApp FrontendApp (Either DbError a)
snapRunAction clearanceAbs action = do
    rn :: MVar SystemRNG <- gets (^. rng)
    st :: AcidState DB <- getAcidState
    _cfg :: ThentosConfig <- gets (^. cfg)
    runAction ((st, rn, _cfg), clearanceAbs) action

snapRunAction' :: ThentosClearance -> Action (MVar SystemRNG) a
      -> Handler FrontendApp FrontendApp (Either DbError a)
snapRunAction' = snapRunAction . const . Right

{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}

module Frontend (runFrontend) where

import Control.Lens (makeLenses, view, (^.))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Acid (AcidState)
import Data.AffineSpace ((.+^))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Data.Monoid ((<>))
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Data.String.Conversions (cs)
import Data.Thyme (getCurrentTime)
import Snap.Blaze (blaze)
import Snap.Core (rqURI, getParam, getsRequest, redirect', parseUrlEncoded, printUrlEncoded, modifyResponse, setResponseStatus, getResponse, finishWith)
import Snap.Http.Server (defaultConfig, setPort)
import Snap.Snaplet (Snaplet, SnapletInit, snapletValue, makeSnaplet, nestSnaplet, addRoutes, Handler)
import Snap.Snaplet.AcidState (Acid, acidInitManual, HasAcid(getAcidStore), update, query)
import Text.Digestive.Snap (runForm)

import DB (AddUser(AddUser), LookupUserByName(..), StartSession(..))
import DB.Protect (thentosPublic)
import DB.Error (DbError(NoSuchUser))
import Frontend.Pages (addUserPage, userForm, userAddedPage, loginForm, loginPage, errorPage)
import Types
import Frontend.Util (serveSnaplet)


data FrontendApp = FrontendApp {_db :: Snaplet (Acid DB)}

makeLenses ''FrontendApp

instance HasAcid FrontendApp DB where
    getAcidStore = view (db . snapletValue)

runFrontend :: Int -> AcidState DB -> IO ()
runFrontend port st = serveSnaplet (setPort port defaultConfig) (frontendApp st)

frontendApp :: AcidState DB -> SnapletInit FrontendApp FrontendApp
frontendApp st = makeSnaplet "Thentos" "The Thentos universal user management system" Nothing $ do
    a <- nestSnaplet "acid" db $ acidInitManual st
    addRoutes routes
    return $ FrontendApp a

routes :: [(ByteString, Handler FrontendApp FrontendApp ())]
routes = [ ("create_user", userAddHandler)
         , ("login", loginHandler)
         ]

userAddHandler :: Handler FrontendApp FrontendApp ()
userAddHandler = do
    (_view, result) <- runForm "create_user" userForm
    case result of
        Nothing -> blaze $ addUserPage _view
        Just user -> do
            liftIO $ print user
            -- FIXME: this doesn't handle errors
            _ <- update (AddUser user thentosPublic)
            blaze userAddedPage

loginHandler :: Handler FrontendApp FrontendApp ()
loginHandler = do
    uri <- getsRequest rqURI
    (_view, result) <- runForm (cs uri) loginForm
    case result of
        Nothing -> blaze $ loginPage _view uri
        Just (name, password) -> do
            eUser <- query $ LookupUserByName name thentosPublic
            case eUser of
                Right (uid, user) ->
                    if password == (user ^. userPassword)
                        then loginSuccess uid
                        else loginFail
                Left NoSuchUser -> loginFail
                Left e -> blaze . errorPage $ show e

  where
    loginFail = blaze "Bad username / password combination"

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
        eSessionToken <- update $ StartSession uid (ServiceId $ cs sid) (TimeStamp now) (TimeStamp $ now .+^ 14 * 24 * 3600) thentosPublic
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

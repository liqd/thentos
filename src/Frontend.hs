{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}

module Frontend (runFrontend) where

import Control.Lens (makeLenses, view, (^.))
import Control.Monad.IO.Class (liftIO)
import Data.Acid (AcidState)
import Data.AffineSpace ((.+^))
import Data.ByteString (ByteString)
import Data.Thyme (getCurrentTime)
import Snap.Blaze (blaze)
import Snap.Http.Server (defaultConfig, setPort)
import Snap.Snaplet (Snaplet, SnapletInit, snapletValue, makeSnaplet, nestSnaplet, addRoutes, Handler)
import Snap.Snaplet.AcidState (Acid, acidInitManual, HasAcid(getAcidStore), update, query)
import Text.Digestive.Snap (runForm)

import DB (AddUser(AddUser), LookupUserByName(..), AddService(..), StartSession(..))
import DB.Protect (thentosPublic)
import DB.Error (DbError(NoSuchUser))
import Frontend.Pages (addUserPage, userForm, userAddedPage, loginForm, loginPage, errorPage)
import Types
import Frontend.Util (serveSnaplet)

import Text.Blaze.Html5 (text)

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
    (view, result) <- runForm "create_user" userForm
    case result of
        Nothing -> blaze $ addUserPage view
        Just user -> do
            liftIO $ print user
            -- FIXME: this doesn't handle errors
            update (AddUser user thentosPublic)
            blaze userAddedPage

loginHandler :: Handler FrontendApp FrontendApp ()
loginHandler = do
    (view, result) <- runForm "login" loginForm
    case result of
        Nothing -> blaze $ loginPage view
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
        -- FIXME: we need to take the service id from a query parameter
        Right sid <- update $ AddService thentosPublic
        now <- liftIO getCurrentTime
        -- FIXME: how long should the session live?
        eSessionToken <- update $ StartSession uid sid (TimeStamp now) (TimeStamp $ now .+^ 14 * 24 * 3600) thentosPublic
        case eSessionToken of
            Left e -> blaze . errorPage $ show e
            Right sessionToken -> blaze . text $ fromSessionToken sessionToken

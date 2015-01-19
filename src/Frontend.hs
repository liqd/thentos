{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}

module Frontend (runFrontend) where

import Control.Lens (makeLenses, view)
import Control.Monad.IO.Class (liftIO)
import Data.Acid (AcidState)
import Data.ByteString (ByteString)

import Snap.Blaze (blaze)
import Snap.Core (Snap, route)
import Snap.Http.Server (httpServe, defaultConfig, setPort)
import Snap.Snaplet (Snaplet, SnapletInit, snapletValue, makeSnaplet, nestSnaplet, serveSnaplet, addRoutes, Handler)
import Snap.Snaplet.AcidState (Acid, acidInitManual, HasAcid(getAcidStore), update)
import Text.Digestive.Snap (runForm)

import DB (AddUser(AddUser))
import DB.Protect (thentosPublic)
import Frontend.Pages (addUserPage, userForm, userAddedPage)
import Types (DB)

data FrontendApp = FrontendApp {_db :: Snaplet (Acid DB)}

makeLenses ''FrontendApp

instance HasAcid FrontendApp DB where
    getAcidStore = view (db . snapletValue)

runFrontend :: Int -> AcidState DB -> IO ()
runFrontend port st = serveSnaplet (setPort port defaultConfig) (frontendApp st)

frontendApp :: AcidState DB ->  SnapletInit FrontendApp FrontendApp
frontendApp st = makeSnaplet "frontendApp" "The frontend snap app" Nothing $ do
    a <- nestSnaplet "acid" db $ acidInitManual st
    addRoutes routes
    return $ FrontendApp a

routes :: [(ByteString, Handler FrontendApp FrontendApp ())]
routes = [ ("create_user.html", userAddHandler)
         ]

userAddHandler :: Handler FrontendApp FrontendApp ()
userAddHandler = do
    (view, result) <- runForm "create_user" userForm
    case result of
        Nothing -> blaze $ addUserPage view
        Just user -> do
            liftIO $ print user
            update (AddUser user thentosPublic)
            blaze userAddedPage

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Site
  ( app
  ) where

import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.CaseInsensitive
import           Data.Monoid
import           Data.String.Conversions
import           Debug.Trace
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import           Snap
import           Snap.Blaze (blaze)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           System.Environment (getArgs)
import           Text.Blaze.Html (Html)

data App = App
type AppHandler = Handler App App

handleApp :: AppHandler ()
handleApp = do
    token <- getParam "token"
    method GET $ blaze (appPage token ())

appPage :: (Show token, Show sessionMetaData) => token -> sessionMetaData -> Html
appPage token sessionMetaData =
    H.docTypeHtml $ do
        H.head $
            H.title "Welcome to the thentos test service!"
        H.body $ do
            H.p ("your session token: " <> H.string (show token))
            H.p ("data sent to us from thentos (session meta data): " <> H.string (show sessionMetaData))
            H.button $ do
                H.text "login"
            H.button $ do
                H.text "logout"

routes :: ByteString -> [(ByteString, Handler App App ())]
routes sid = [ ("/app", handleApp)
           , ("/login", helloWorldLogin sid)
           , ("",     serveDirectory "static")  -- for css and what not.
           ]

app :: SnapletInit App App
app = makeSnaplet "app" "A hello-world service for testing thentos." Nothing $ do
    args <- liftIO $ getArgs
    liftIO . putStrLn $ show args
    let sid = BC.pack $ head args
    addRoutes (routes sid)
    return $ App

helloWorldLogin serviceId =
    redirect'
        ("http://localhost:8002/login?sid=" <> serviceId <> "&redirect="
            <> urlEncode "http://localhost:8000/app?foo=bar")
        303

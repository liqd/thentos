{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Site
  ( app
  ) where

import           Control.Applicative
import           Data.ByteString (ByteString)
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
import           Text.Blaze.Html (Html)

data App = App
type AppHandler = Handler App App

handleApp :: Handler App App ()
handleApp = do
    token :: Maybe [ByteString] <- getHeaders (mk "X-Thentos-Token") <$> getRequest
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

routes :: [(ByteString, Handler App App ())]
routes = [ ("/app", handleApp)
         , ("",     serveDirectory "static")  -- for css and what not.
         ]

app :: SnapletInit App App
app = makeSnaplet "app" "A hello-world service for testing thentos." Nothing $ do
    addRoutes routes
    return $ App

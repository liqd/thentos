{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Site
  ( app
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Network.HTTP.Client.Conduit (parseUrl, httpLbs, responseBody, requestHeaders, withManager)
import Snap (Handler, SnapletInit, makeSnaplet, redirect, redirect', urlEncode, gets, liftIO, getParam, method, Method(GET), ifTop, addRoutes)
import Snap.Blaze (blaze)
import Snap.Util.FileServe (serveDirectory)
import Text.Blaze.Html (Html)
import Text.Show.Pretty (ppShow)

import qualified Data.ByteString.Char8 as BC
import qualified Data.Configurator as Configurator
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA


data App = App { aHWConfig :: HWConfig }
type AppHandler = Handler App App

data HWConfig =
    HWConfig
      { thentosBackendUrl  :: ByteString
      , thentosFrontendUrl :: ByteString
      , helloWorldUrl      :: ByteString
      , serviceId          :: ByteString
      , serviceKey         :: ByteString
      }
  deriving (Eq, Show)


handleApp :: AppHandler ()
handleApp = do
    token <- getParam "token"
    tokenIsOk <- tokenOk token
    method GET $ blaze (appPage token () tokenIsOk)

appPage :: Show sessionMetaData => Maybe ByteString -> sessionMetaData -> Bool -> Html
appPage token sessionMetaData isTokenOk =
    H.docTypeHtml $ do
        H.head $
            H.title "Welcome to the thentos test service!"
        H.body $ do
            H.p $ "your session token: " <> H.string (show token)
            H.p $ "Token ok: " <> H.string (show isTokenOk) <> " (checked with thentos)"
            H.p $ "data sent to us from thentos (session meta data): " <> H.string (show sessionMetaData)
            H.p $ H.a H.! HA.href (H.toValue . BC.unpack $ "/login") $ do
                H.text "login"
            H.p $ H.a H.! HA.href (H.toValue . BC.unpack $ "/logout") $ do
                H.text "logout"

routes :: HWConfig -> [(ByteString, Handler App App ())]
routes hwConfig =
      [ ("",       ifTop $ redirect "/app")
      , ("/app",   handleApp)
      , ("/login", helloWorldLogin hwConfig)
      , ("",       serveDirectory "static")  -- for css and what not.
      ]

app :: SnapletInit App App
app = makeSnaplet "app" "A hello-world service for testing thentos." Nothing $ do
    Just hwConfig <- liftIO $ loadConfig
    liftIO . putStrLn $ ppShow hwConfig
    addRoutes (routes hwConfig)
    return $ App hwConfig
  where
    loadConfig :: IO (Maybe HWConfig)
    loadConfig = do
        config <- Configurator.load [Configurator.Required "devel.config"]
        HWConfig <$$>
            Configurator.lookup config "thentos_backend_url" <**>
            Configurator.lookup config "thentos_frontend_url" <**>
            Configurator.lookup config "hello_world_url" <**>
            Configurator.lookup config "service_id" <**>
            Configurator.lookup config "service_key"

    -- function-infix provides (<$$>), but not (<**>).  While waiting
    -- for somebody to write a package applicative-infix, we define
    -- these inline, with slightly more specific types.  (NOTE:
    -- Control.Applicative.<**> exists, but does something else.)

    (<$$>) :: (a -> b) -> IO (Maybe a) -> IO (Maybe b)
    a <$$> b = do y <- b; return $ a <$> y

    (<**>) :: IO (Maybe (a -> b)) -> IO (Maybe a) -> IO (Maybe b)
    a <**> b = do x <- a; y <- b; return $ x <*> y

helloWorldLogin :: HWConfig -> Handler App App ()
helloWorldLogin hwConfig =
    redirect'
        (thentosFrontendUrl hwConfig <> "/login?sid=" <> (serviceId hwConfig) <> "&redirect="
            <> urlEncode (helloWorldUrl hwConfig <> "/app?foo=bar"))
        303

tokenOk :: Maybe ByteString -> Handler App App Bool
tokenOk Nothing = return False
tokenOk (Just token) = do
    hwConfig <- gets aHWConfig
    let sid = serviceId hwConfig
        key = serviceKey hwConfig
        url = thentosBackendUrl hwConfig <> "/session/" <> token
    liftIO . withManager $ do
        initReq <- parseUrl $ BC.unpack url
        let req = initReq
                    { requestHeaders = [ ("X-Thentos-Password", key)
                                       , ("X-Thentos-Service", sid)
                                       ]
                    }
        response <- httpLbs req
        case responseBody response of
            "true"  -> return True
            "false" -> return False
            e       -> fail $ "Bad response: " ++ show e

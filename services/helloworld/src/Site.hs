{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Site
  ( app
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson ((.=))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Network.HTTP.Client.Conduit (parseUrl, httpLbs, responseBody, requestHeaders, requestBody, withManager, RequestBody(RequestBodyLBS))
import Network.HTTP.Types (methodPost)
import Snap (Handler, SnapletInit, makeSnaplet, redirect, redirect', urlEncode, gets, liftIO, getParam, method, Method(GET), ifTop, addRoutes)
import Snap.Blaze (blaze)
import Snap.Util.FileServe (serveDirectory)
import Text.Blaze.Html (Html)
import Text.Show.Pretty (ppShow)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.Configurator as Configurator
import qualified Network.HTTP.Client.Conduit as HC
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA


data App = App { aHWConfig :: HWConfig }
type AppHandler = Handler App App

data HWConfig =
    HWConfig
      { thentosBackendUrl  :: ByteString
      , thentosFrontendUrl :: ByteString
      , helloWorldUrl      :: ByteString
      , serviceId          :: Text
      , serviceKey         :: Text
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
            H.p $ "Token ok (checked with thentos): " <> H.string (show isTokenOk)
            H.p $ "data sent to us from thentos (session meta data): " <> H.string (show sessionMetaData)
            H.p $ H.a H.! HA.href (H.toValue . BC.unpack $ "/login") $ do
                H.text "login"
            H.p $ H.a H.! HA.href (H.toValue . BC.unpack $ "/logout") $ do
                H.text "logout"

routes :: [(ByteString, Handler App App ())]
routes =
      [ ("",        ifTop $ redirect "/app")
      , ("/app",    handleApp)
      , ("/login",  helloWorldLogin)
      , ("/logout", helloWorldLogout)
      , ("",        serveDirectory "static")  -- for css and what not.
      ]

app :: SnapletInit App App
app = makeSnaplet "app" "A hello-world service for testing thentos." Nothing $ do
    Just hwConfig <- liftIO $ loadConfig
    liftIO . putStrLn $ ppShow hwConfig
    addRoutes routes
    return $ App hwConfig
  where
    loadConfig :: IO (Maybe HWConfig)
    loadConfig = do
        config <- Configurator.load [Configurator.Required "devel.config"]
          -- (config file is hard-coded, but that's ok.  this is just
          -- trying to be a helloworld app to test and demonstrate
          -- some thentos concepts, not production code.)

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

helloWorldLogin :: Handler App App ()
helloWorldLogin = do
    hwConfig <- gets aHWConfig
    redirect'
        (thentosFrontendUrl hwConfig <> "/log_into_service?sid=" <> (urlEncode . encodeUtf8 $ serviceId hwConfig) <> "&redirect="
            <> urlEncode (helloWorldUrl hwConfig <> "/app"))
        303

-- | FIXME: notify thentos that user is logged out of service.  this
-- can happen either directly between service and thentos, or via
-- redirect through the browser.
helloWorldLogout :: Handler App App ()
helloWorldLogout = redirect' "/app" 303

tokenOk :: Maybe ByteString -> Handler App App Bool
tokenOk Nothing = return False
tokenOk (Just tokBS) =
    case decodeUtf8' tokBS of
        Left _ -> return False
        Right token -> do
            hwConfig <- gets aHWConfig
            let sid = encodeUtf8 $ serviceId hwConfig
                url = thentosBackendUrl hwConfig <> "/session"
                token_obj = Aeson.object ["fromSessionToken" .= token]
                reqBody = RequestBodyLBS $ Aeson.encode token_obj
            liftIO . withManager $ do
                initReq <- parseUrl $ BC.unpack url
                let req = initReq
                            { requestHeaders = [ ("X-Thentos-Service", sid) ]
                            , requestBody = reqBody
                            }
                response <- httpLbs req
                case responseBody response of
                    "true"  -> return True
                    "false" -> return False
                    e       -> fail $ "Bad response: " ++ show e

-- this is currently unused, but we should really have an example where
-- the service has to authenticate with thentos
getServiceSessionToken :: Handler App App ByteString
getServiceSessionToken = do
    hwConfig <- gets aHWConfig
    let sid = serviceId hwConfig
        key = serviceKey hwConfig
        url = thentosBackendUrl hwConfig <> "/session"
        json_sid = Aeson.object ["fromServiceId" .= sid]
        json_key = Aeson.object ["fromServiceKey" .= key]
        reqBody = RequestBodyLBS $ Aeson.encode [json_sid, json_key]
    initReq <- liftIO $ parseUrl (BC.unpack url)

    liftIO . withManager $ do
        let req = initReq { requestBody = reqBody, HC.method = methodPost }
        response <- httpLbs req
        return . toStrict $ responseBody response

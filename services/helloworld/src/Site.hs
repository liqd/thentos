{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Site
  ( app
  ) where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Lens (makeLenses, (^.))
import Control.Monad (when)
import Control.Monad.State.Class (gets)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Network.HTTP.Client.Conduit (parseUrl, httpLbs, responseBody, requestHeaders, requestBody, withManager, RequestBody(RequestBodyLBS))
import Network.HTTP.Types (methodPost, methodDelete)
import Snap (Handler, SnapletInit, makeSnaplet, redirect, redirect', urlEncode, getParam, method, Method(GET), ifTop, addRoutes)
import Snap.Snaplet (Snaplet, with, nestSnaplet)
import Snap.Snaplet.Session (getFromSession, setInSession, commitSession, resetSession)
import Snap.Snaplet.Session.Backends.CookieSession (initCookieSessionManager)
import Snap.Snaplet.Session.SessionManager (SessionManager)
import Snap.Blaze (blaze)
import Snap.Util.FileServe (serveDirectory)
import Text.Blaze.Html (Html)
import Text.Show.Pretty (ppShow)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.Configurator as Configurator
import qualified Data.HashMap.Strict as HashMap
import qualified Network.HTTP.Client.Conduit as HC
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA


data App = App { _aHWConfig :: HWConfig
               , _sess      :: Snaplet SessionManager
               }
type AppHandler = Handler App App

configFilePath :: FilePath
configFilePath = "devel.config"

-- | The snap web server is configured via "Snap.Http.Server.Config".
-- The field 'helloWorldUrl' in this app-config type contains the
-- exposed url (e.g. for sending out links to the browser).  Since we
-- have no control over the network topology here (proxy setup and
-- all), it needs to be checked during system configuration that the
-- two work together.
data HWConfig =
    HWConfig
      { thentosBackendUrl  :: ByteString
      , thentosFrontendUrl :: ByteString
      , helloWorldUrl      :: ByteString
      , serviceId          :: Text
      , serviceKey         :: Text
      }
  deriving (Eq, Show)

makeLenses ''App

handleApp :: AppHandler ()
handleApp = do
    mToken <- with sess $ getFromSession "sessiontoken"
    tokenIsOk <- verifyToken mToken
    meta <- fromMaybe (return "")
                      (if tokenIsOk then getMetadata <$> mToken else Nothing)
    let json_obj = Aeson.decode $ fromStrict meta
        mUsername = json_obj >>= HashMap.lookup ("servSessMDUser" :: Text)
        username = fromMaybe "ERROR: couldn't parse username" mUsername
    method GET . blaze $ appPage mToken tokenIsOk meta username

appPage :: Show sessionMetadata => Maybe Text -> Bool -> sessionMetadata -> String -> Html
appPage token isTokenOk sessionMetadata user =
    H.docTypeHtml $ do
        H.head $ do
            H.title "Greetotron2000"
            H.link H.! HA.rel "stylesheet" H.! HA.href "screen.css"
        H.body $ do
            H.h1 "Greetotron2000"

            case token of
                (Just _) -> H.div H.! HA.class_ "logged_in" $ do
                    H.p $ "you are logged in.  hello, " <> H.string user <> "!"
                    H.p $ H.a H.! HA.href (H.toValue . BC.unpack $ "/logout") $ H.text "logout"
                Nothing -> H.div H.! HA.class_ "logged_out" $ do
                    H.p "hello, nobody!"
                    H.p "please log in if you want to be greeted properly."
                    H.p $ H.a H.! HA.href (H.toValue . BC.unpack $ "/login") $ H.text "login via thentos"

            H.hr
            H.table $ do
                H.tr $ do
                    H.td "session token"
                    H.td . H.pre . H.string . ppShow $ token
                H.tr $ do
                    H.td "verified"
                    H.td . H.pre . H.string . ppShow $ isTokenOk
                H.tr $ do
                    H.td "session meta data"
                    H.td . H.pre . H.string . ppShow $ sessionMetadata


routes :: [(ByteString, Handler App App ())]
routes =
      [ ("",                ifTop $ redirect "/app")
      , ("/app",            handleApp)
      , ("/login",          helloWorldLogin)
      , ("/logout",         helloWorldLogout)
      , ("/login_callback", helloWorldLoginCallback)
      , ("",                serveDirectory "static")  -- for css and what not.
      ]

app :: SnapletInit App App
app = makeSnaplet "app" "A hello-world service for testing thentos." Nothing $ do
    Just hwConfig <- liftIO $ loadConfig
    liftIO . putStrLn $ ppShow hwConfig
    addRoutes routes
    App
       <$> pure hwConfig
       <*> (nestSnaplet "hwsess" sess $
               initCookieSessionManager "site_key.txt" "hwsess" Nothing)
  where
    loadConfig :: IO (Maybe HWConfig)
    loadConfig = do
        config <- Configurator.load [Configurator.Required configFilePath]
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
    hwConfig <- gets (^. aHWConfig)
    redirect'
        (thentosFrontendUrl hwConfig <> "/service/login?sid=" <> (urlEncode . encodeUtf8 $ serviceId hwConfig) <> "&redirect="
            <> urlEncode (helloWorldUrl hwConfig <> "/login_callback"))
        303

helloWorldLoginCallback :: AppHandler ()
helloWorldLoginCallback = do
    mTokenBS <- getParam "token"
    case decodeUtf8' <$> mTokenBS of
        Just (Right token) -> do
            tokenIsOk <- verifyToken (Just token)
            when tokenIsOk $ with sess $ do
                setInSession "sessiontoken" token
                commitSession
        _ -> return ()
    redirect "/app"

helloWorldLogout :: Handler App App ()
helloWorldLogout = do
    mToken <- with sess $ getFromSession "sessiontoken"
    case mToken of
        Just token -> do
            initReq <- makeRequest ("/service_session/" <> urlEncode (encodeUtf8 token))
            let req = initReq { HC.method = methodDelete }
            liftIO . withManager $ httpLbs req
            with sess (resetSession >> commitSession)
        Nothing -> return ()
    redirect' "/app" 303

verifyToken :: Maybe Text -> Handler App App Bool
verifyToken Nothing = return False
verifyToken (Just token) = do
    hwConfig <- gets (^. aHWConfig)
    req <- makeRequest $ "/service_session/" <> urlEncode (encodeUtf8 token)
    liftIO . withManager $ do
        response <- httpLbs req
        case responseBody response of
            "true"  -> return True
            "false" -> return False
            e       -> fail $ "Bad response: " ++ show e

getMetadata :: Text -> Handler App App ByteString
getMetadata token = do
    let path = "/service_session/" <> urlEncode (encodeUtf8 token) <> "/meta"
    req <- makeRequest path
    liftIO . withManager $ do
        response <- httpLbs req
        return . toStrict $ responseBody response

makeRequest :: ByteString -> Handler App App HC.Request
makeRequest path = do
    hwConfig <- gets (^. aHWConfig)
    let url = thentosBackendUrl hwConfig <> path
    initReq <- liftIO . parseUrl $ BC.unpack url
    let sid = encodeUtf8 $ serviceId hwConfig
    return $ initReq { requestHeaders = [("X-Thentos-Service", sid), ("Content-Type", "application/json")] }

-- this is currently unused, but we should really have an example where
-- the service has to authenticate with thentos
getServiceSessionToken :: Handler App App ByteString
getServiceSessionToken = do
    hwConfig <- gets (^. aHWConfig)
    let sid = serviceId hwConfig
        key = serviceKey hwConfig
        url = thentosBackendUrl hwConfig <> "/session"
        json_sid = Aeson.object ["fromServiceId" .= sid]
        json_key = Aeson.object ["fromServiceKey" .= key]
        reqBody = RequestBodyLBS $ Aeson.encode [json_sid, json_key]
    initReq <- liftIO $ parseUrl (BC.unpack url)

    liftIO . withManager $ do
        let req = initReq { requestBody = reqBody, HC.method = methodPost }  -- FIXME: this won't work (need more headers!)
        response <- httpLbs req
        return . toStrict $ responseBody response

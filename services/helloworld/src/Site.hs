{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Site
  ( app
  ) where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Lens (makeLenses, (^.))
import Control.Monad (when)
import Control.Exception (SomeException, catch, bracket_)
import Control.Monad.State.Class (gets)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, withObject, withText, (.=))
import Data.String.Conversions (SBS, LBS, cs, (<>))
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
import System.IO (hPutStrLn, stderr)

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
      { thentosBackendUrl  :: SBS
      , thentosFrontendUrl :: SBS
      , helloWorldUrl      :: SBS
      , serviceId          :: Text
      , serviceKey         :: Text
      }
  deriving (Eq, Show)

makeLenses ''App

handleApp :: AppHandler ()
handleApp = do
    mToken    <- with sess $ getFromSession "sessiontoken"
    tokenIsOk <- verifyToken mToken
    meta      <- maybe (return Nothing) getMetadata mToken
    method GET . blaze $ appPage mToken tokenIsOk meta

appPage :: Maybe Text -> Bool -> Maybe HwMetadata -> Html
appPage token isTokenOk meta =
    H.docTypeHtml $ do
        H.head $ do
            H.title "Greetotron2000"
            H.link H.! HA.rel "stylesheet" H.! HA.href "screen.css"
        H.body $ do
            H.h1 "Greetotron2000"

            case meta of
                (Just (HwMetadata _ user)) -> H.div H.! HA.class_ "logged_in" $ do
                    H.p $ "you are logged in.  hello, " <> H.text user <> "!"
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
                    H.td . H.pre . H.string . ppShow $ meta


routes :: [(SBS, Handler App App ())]
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
            req <- do
                let body = makeJsonServiceSessionToken token
                initReq <- makeRequest "/service_session/" body
                return $ initReq { HC.method = methodDelete }
            runRequest req
            with sess (resetSession >> commitSession)
        Nothing -> return ()
    redirect' "/app" 303

verifyToken :: Maybe Text -> Handler App App Bool
verifyToken Nothing = return False
verifyToken (Just token) = do
    liftIO $ print ("verifyToken" :: SBS)
    let body = makeJsonServiceSessionToken token
    req <- makeRequest "/service_session/" body
    liftIO $ print req
    liftIO . withManager $ do
        response <- runRequest req
        liftIO $ print (response, responseBody response)
        case responseBody response of
            "true"  -> return True
            "false" -> return False
            e       -> fail $ "Bad response: " ++ show e

makeJsonServiceSessionToken :: Text -> RequestBody
makeJsonServiceSessionToken token =
    let json_token = Aeson.object ["fromServiceSessionToken" .= token]
    in RequestBodyLBS $ Aeson.encode json_token

data HwMetadata = HwMetadata Aeson.Value Text
  deriving (Eq, Show)

instance FromJSON HwMetadata where
    parseJSON v = flip (withObject msg) v $ \ obj -> case HashMap.lookup "srvSessMdUser" obj of
        Nothing -> fail "no srvSessMdUser field."
        Just t  -> withText msg (return . HwMetadata v) t
      where
        msg = "instance FromJSON (HwMetadata Aeson.Value Text)"


getMetadata :: Text -> Handler App App (Maybe HwMetadata)
getMetadata token = do
    let body = makeJsonServiceSessionToken token
    req <- makeRequest "/service_session/meta" body
    response <- runRequest req
    return . Aeson.decode $ responseBody response

makeRequest :: SBS -> RequestBody -> Handler App App HC.Request
makeRequest path body = do
    hwConfig <- gets (^. aHWConfig)
    let url = thentosBackendUrl hwConfig <> path
    initReq <- liftIO . parseUrl $ BC.unpack url
    let sid = encodeUtf8 $ serviceId hwConfig
    return $ initReq
        { requestHeaders = [("X-Thentos-Service", sid), ("Content-Type", "application/json")]
        , requestBody = body
        }

-- | httpLbs from http-conduit likes to throw exceptions a lot, and snap likes to pass them on the
-- browser rather than log them to stderr.  this function is for handling exceptions better, even
-- though currently it doesn't to all that great of a job.
runRequest :: MonadIO m => HC.Request -> m (HC.Response LBS)
runRequest req = liftIO $ bracket_
    (return ())
    (return ())
    (withManager (httpLbs req) `catch` (\ (e :: SomeException) -> let s = "*** " ++ ppShow e in hPutStrLn stderr s >> error s))


-- this is currently unused, but we should really have an example where
-- the service has to authenticate with thentos
getServiceSessionToken :: Handler App App SBS
getServiceSessionToken = do
    hwConfig <- gets (^. aHWConfig)
    let sid = serviceId hwConfig
        key = serviceKey hwConfig
        url = thentosBackendUrl hwConfig <> "/session"
        json_sid = Aeson.object ["fromServiceId" .= sid]
        json_key = Aeson.object ["fromServiceKey" .= key]
        reqBody = RequestBodyLBS $ Aeson.encode [json_sid, json_key]
    initReq <- liftIO $ parseUrl (BC.unpack url)

    let req = initReq { requestBody = reqBody, HC.method = methodPost }  -- FIXME: this won't work (need more headers!)
    response <- runRequest req
    return . cs $ responseBody response

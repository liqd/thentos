{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Thentos.Frontend.Util where

import Snap (Snap, Config, getOther, SnapletInit, runSnaplet, when, liftIO, combineConfig, getVerbose)
import Snap.Http.Server (simpleHttpServe)
import Snap.Snaplet.Config (AppConfig(appEnvironment))
import Control.Monad.CatchIO (try)
import qualified Data.Text as T
import System.IO (stderr, hPutStrLn)
import System.Directory (createDirectoryIfMissing)
import Control.Exception (SomeException)
import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.))
import Control.Monad.State.Class (gets)
import Crypto.Random (SystemRNG)
import Data.Acid (AcidState)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.Monoid ((<>))
import Data.String.Conversions (SBS, ST, cs)
import Data.Text.Encoding (encodeUtf8)
import Snap.Blaze (blaze)
import Snap.Core (getResponse, finishWith, urlEncode)
import Snap.Core (rqURI, getsRequest, redirect', modifyResponse, setResponseStatus)
import Snap.Snaplet.AcidState (getAcidState)
import Snap.Snaplet.Session (commitSession, setInSession, getFromSession)
import Snap.Snaplet (Handler, with)
import System.Log.Missing (logger)
import System.Log (Priority(DEBUG, CRITICAL))
import Text.Digestive.Form (Form)
import Text.Digestive.Snap (runForm)
import Text.Digestive.View (View)
import URI.ByteString (parseURI, parseRelativeRef, laxURIParserOptions, serializeURI, serializeRelativeRef)
import URI.ByteString (URI(..), RelativeRef(..), URIParserOptions, Query(..))

import qualified Data.Aeson as Aeson
import qualified Text.Blaze.Html5 as H

import Thentos.Api
import Thentos.Config
import Thentos.DB
import Thentos.Frontend.Pages
import Thentos.Frontend.Types
import Thentos.Types
import Thentos.Util


-- * dashboard construction

-- | Call 'buildDashboard' to consruct a dashboard page and render it
-- into the application monad.
renderDashboard :: DashboardTab -> (User -> [Role] -> H.Html) -> FH ()
renderDashboard tab pagelet = buildDashboard tab pagelet >>= blaze

-- | Like 'renderDashboard', but take a pagelet builder instead of a
-- pagelet.
renderDashboard' :: DashboardTab -> (User -> [Role] -> H.Html) -> FH ()
renderDashboard' tab pagelet = buildDashboard tab pagelet >>= blaze

-- | Take a dashboard tab and a pagelet, and consruct the dashboard
-- page.
buildDashboard :: DashboardTab -> (User -> [Role] -> H.Html) -> FH H.Html
buildDashboard tab pagelet = buildDashboard' tab (\ u -> return . pagelet u)

-- | Like 'buildDashboard', but take a pagelet builder instead of a
-- pagelet.
buildDashboard' :: DashboardTab -> (User -> [Role] -> FH H.Html) -> FH H.Html
buildDashboard' tab pageletBuilder = do
    runAsUser $ \ clearance session -> do
        msgs <- popAllFrontendMsgs
        let uid = fsdUser session
        eUser  <- snapRunAction' clearance . queryAction $ LookupUser uid
        eRoles <- snapRunAction' clearance . queryAction $ LookupAgentRoles (UserA uid)
        case (eUser, eRoles) of
            (Right (_, user), Right roles) -> dashboardPagelet msgs roles tab <$> pageletBuilder user roles
            _ -> error "unreachable"
                 -- FIXME: error handling.  (we need a better approach for this in general!)


-- * form rendering and processing

-- | Take a form action string, a form, a pagelet matching the form
-- and a dashboard tab to render it in, and an action to be performed
-- on the form data once submitted.  Depending on the 'runForm'
-- result, either render the form or process it.  The formAction
-- passed to 'runForm' is the URI of the current request.
runPageletForm :: forall v a .
       Form v FH a
    -> (ST -> View v -> User -> [Role] -> H.Html) -> DashboardTab
    -> (a -> FH ())
    -> FH ()
runPageletForm f pagelet = runPageletForm' f (\ formAction v u -> return . pagelet formAction v u)

-- | Like 'runPageletForm', but takes a page builder instead of a
-- page (this is more for internal use).
runPageletForm' :: forall v a .
       Form v FH a
    -> (ST -> View v -> User -> [Role] -> FH H.Html) -> DashboardTab
    -> (a -> FH ())
    -> FH ()
runPageletForm' f buildPagelet tab = runPageForm' f buildPage
  where
    buildPage :: ST -> View v -> FH H.Html
    buildPage formAction = buildDashboard' tab . buildPagelet formAction

-- | Full-page version of 'runPageletForm'.
runPageForm :: forall v a .
       Form v FH a
    -> (ST -> View v -> H.Html)
    -> (a -> FH ())
    -> FH ()
runPageForm f page = runPageForm' f (\ formAction -> return . page formAction)

-- | Full-page version of 'runPageletForm''.
runPageForm' :: forall v a .
       Form v FH a
    -> (ST -> View v -> FH H.Html)
    -> (a -> FH ())
    -> FH ()
runPageForm' f buildPage a = do
    formAction <- cs <$> getsRequest rqURI
    (view, mResult) <- logger DEBUG "[formDriver: runForm]" >> runForm formAction f
    case mResult of
        Nothing -> buildPage formAction view >>= blaze
        Just result -> logger DEBUG "[formDriver: action]" >> a result


-- * authentication

-- | Runs a given handler with the credentials and the session data of the
-- currently logged-in user
runAsUser :: (ThentosClearance -> FrontendSessionData -> FH a) -> FH a
runAsUser handler = do
    mSessionData <- getSessionData
    case mSessionData of
        Nothing -> redirect' "/user/login" 303
        Just sessionData -> do
            Right clearance <- snapRunAction' allowEverything $ getUserClearance (fsdUser sessionData)
            handler clearance sessionData
 where
    getSessionData :: FH (Maybe FrontendSessionData)
    getSessionData = with sess $ do
        mSessionDataBS <- getFromSession "sessionData"
        return $ mSessionDataBS >>= Aeson.decode . cs


-- * session management

-- | This is not race-condition-free, but the session only lives in
-- the single thread that handles the single request, so thread-safety
-- is not required.  (FIXME: think about this some more.)
updateSessionData :: (FrontendSessionData -> (FrontendSessionData, a)) -> FH a
updateSessionData op = with sess $ do
    mSessionData <- (>>= Aeson.decode . cs) <$> getFromSession "sessionData"
    case mSessionData of
        Just sessionData -> do
            let (sessionData', val) = op sessionData
            setInSession "sessionData" . cs . Aeson.encode $ sessionData'
            commitSession
            return val
        Nothing -> crash 500 "No session data."

sendFrontendMsgs :: [FrontendMsg] -> FH ()
sendFrontendMsgs msgs = updateSessionData $ \ fsd -> (fsd { fsdMessages = fsdMessages fsd ++ msgs }, ())

sendFrontendMsg :: FrontendMsg -> FH ()
sendFrontendMsg = sendFrontendMsgs . (:[])

popAllFrontendMsgs :: FH [FrontendMsg]
popAllFrontendMsgs = updateSessionData $ \ fsd -> (fsd { fsdMessages = [] }, fsdMessages fsd)


-- * error handling

crash' :: (Show a) => Int -> a -> SBS -> Handler b v x
crash' status logMsg usrMsg = do
    logger DEBUG $ show (status, logMsg, usrMsg)
    modifyResponse $ setResponseStatus status usrMsg
    blaze . errorPage . cs $ usrMsg
    getResponse >>= finishWith

crash :: Int -> SBS -> Handler b v x
crash status usrMsg = crash' status () usrMsg


urlConfirm :: HttpConfig -> ST -> ST -> ST
urlConfirm feConfig path token = exposeUrl feConfig <//> toST ref
  where
    ref   = RelativeRef Nothing (cs path) (Query query) Nothing
    query = [("token", urlEncode . encodeUtf8 $ token)]
    toST  = cs . toLazyByteString . serializeRelativeRef


-- * uri manipulation

redirectURI :: URI -> FH ()
redirectURI ref = redirect' (cs . toLazyByteString . serializeURI $ ref) 303

redirectRR :: RelativeRef -> FH ()
redirectRR ref = redirect' (cs . toLazyByteString . serializeRelativeRef $ ref) 303


tweakRelativeRqRef :: (RelativeRef -> RelativeRef) -> FH SBS
tweakRelativeRqRef tweak = getsRequest rqURI >>= tweakRelativeRef tweak

tweakRelativeRef :: (RelativeRef -> RelativeRef) -> SBS -> FH SBS
tweakRelativeRef = _tweakURI parseRelativeRef serializeRelativeRef

tweakURI :: (URI -> URI) -> SBS -> FH SBS
tweakURI = _tweakURI parseURI serializeURI

_tweakURI :: forall e t t' . (Show e) =>
                   (URIParserOptions -> SBS -> Either e t)
                -> (t' -> Builder)
                -> (t -> t')
                -> SBS
                -> FH SBS
_tweakURI parse serialize tweak uriBS = do
    let ok uri = do
            return (cs . toLazyByteString . serialize . tweak $ uri)
        er err = do
            logger CRITICAL $ show (err, uriBS)
            crash 500 ("bad request uri: " <> cs (show uriBS))
    either er ok $ parse laxURIParserOptions uriBS


snapRunAction :: (DB -> Timestamp -> Either ThentosError ThentosClearance) -> Action (MVar SystemRNG) a
      -> FH (Either ThentosError a)
snapRunAction clearanceAbs action = do
    rn :: MVar SystemRNG <- gets (^. rng)
    st :: AcidState DB <- getAcidState
    _cfg :: ThentosConfig <- gets (^. cfg)
    runAction ((st, rn, _cfg), clearanceAbs) action

snapRunAction' :: ThentosClearance -> Action (MVar SystemRNG) a
      -> FH (Either ThentosError a)
snapRunAction' clearance = snapRunAction (\ _ _ -> Right clearance)


-- * missing in snap

-- | This does the same as serveSnaplet from the snap package, except that
-- it does not try to read app arguments from the command line
--
-- FIXME: there was some discussion over at snap on how to move that
-- upstream.  what's the state on that?  i think we will have to wait
-- for release 1.0?
serveSnaplet :: Config Snap AppConfig
                 -- ^ The configuration of the server - you can usually pass a
                 -- default 'Config' via
                 -- 'Snap.Http.Server.Config.defaultConfig'.
             -> SnapletInit b b
                 -- ^ The snaplet initializer function.
             -> IO ()
serveSnaplet config initializer = do
    let env = appEnvironment =<< getOther config
    (msgs, handler, doCleanup) <- runSnaplet env initializer

    (conf, site) <- combineConfig config handler
    createDirectoryIfMissing False "log"
    let serve = simpleHttpServe conf

    when (loggingEnabled conf) . liftIO . hPutStrLn stderr $ T.unpack msgs
    _ <- try (serve site)
         :: IO (Either SomeException ())
    doCleanup
  where
    loggingEnabled = not . (== Just False) . getVerbose

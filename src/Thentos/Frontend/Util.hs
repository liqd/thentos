{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}

module Thentos.Frontend.Util where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar)
import Control.Exception (SomeException)
import Control.Lens ((^.), (%~), (.~))
import Control.Monad.CatchIO (try)
import Control.Monad.State.Class (gets)
import Crypto.Random (SystemRNG)
import Data.Acid (AcidState)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (SBS, ST, cs)
import Data.Text.Encoding (encodeUtf8)
import Snap.Blaze (blaze)
import Snap.Core (getResponse, finishWith, urlEncode, getParam)
import Snap.Core (rqURI, getsRequest, redirect', modifyResponse, setResponseStatus)
import Snap.Http.Server (simpleHttpServe)
import Snap (Snap, Config, getOther, SnapletInit, runSnaplet, when, liftIO, combineConfig, getVerbose)
import Snap.Snaplet.AcidState (getAcidState)
import Snap.Snaplet.Config (AppConfig(appEnvironment))
import Snap.Snaplet (Handler, with)
import Snap.Snaplet.Session (commitSession, setInSession, getFromSession)
import System.Directory (createDirectoryIfMissing)
import System.IO (stderr, hPutStrLn)
import System.Log.Missing (logger)
import System.Log (Priority(DEBUG, CRITICAL))
import Text.Digestive.Form (Form)
import Text.Digestive.Snap (runForm)
import Text.Digestive.View (View)
import URI.ByteString (parseURI, parseRelativeRef, laxURIParserOptions, serializeURI, serializeRelativeRef)
import URI.ByteString (URI(..), RelativeRef(..), URIParserOptions, Query(..))

import qualified Data.Aeson as Aeson
import qualified Data.Text as ST
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
    runAsUser $ \ clearance _ sessionLoginData -> do
        msgs <- popAllFrontendMsgs
        let uid = sessionLoginData ^. fslUserId
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
runPageForm' f buildPage a = runHandlerForm f handler a
  where
    handler :: ST -> View v -> FH ()
    handler formAction view = buildPage formAction view >>= blaze

-- | Version of of 'runPageletForm'' that takes a handler rather than
-- a pagelet, and calls that in order to render the empty form.  (For
-- symmetry, the function name should be primed, but there is no
-- non-monadic way to call a handler, so there is only one version of
-- @runHandlerForm@.)
runHandlerForm :: forall v a b .
       Form v FH a
    -> (ST -> View v -> FH b)
    -> (a -> FH b)
    -> FH b
runHandlerForm f handler a = do
    formAction <- cs <$> getsRequest rqURI
    (view, mResult) <- logger DEBUG "[formDriver: runForm]" >> runForm formAction f
    case mResult of
        Nothing -> handler formAction view
        Just result -> logger DEBUG "[formDriver: action]" >> a result


-- * authentication

-- | Call 'runAsUserOrNot', and redirect to login page if not logged
-- in.
runAsUser :: (ThentosClearance -> FrontendSessionData -> FrontendSessionLoginData -> FH a) -> FH a
runAsUser = (`runAsUserOrNot` redirect' "/user/login" 303)

-- | Runs a given handler with the credentials and the session data of
-- the currently logged-in user.  If not logged in, call a default
-- handler that runs without any special clearance.
--
-- FIXME: this currently does not check the session token against the
-- database.  it is trivial to construct a session cookie that gives
-- you user privileges for any user with a known uid, without knowing
-- any further credentials.
runAsUserOrNot :: (ThentosClearance -> FrontendSessionData -> FrontendSessionLoginData -> FH a) -> FH a -> FH a
runAsUserOrNot loggedInHandler loggedOutHandler = do
    sessionData :: FrontendSessionData <- getSessionData
    case sessionData ^. fsdLogin of
        Just sessionLoginData -> do
            Right clearance <- snapRunAction' allowEverything $ getUserClearance (sessionLoginData ^. fslUserId)
            loggedInHandler clearance sessionData sessionLoginData
        Nothing -> loggedOutHandler


-- * session management

-- | Extract 'FrontendSessionData' object from cookie.  If n/a, return
-- an empty one.  If a value is stored, but cannot be decoded, crash.
getSessionData :: FH FrontendSessionData
getSessionData = fromMaybe emptyFrontendSessionData
               . (>>= Aeson.decode . cs)
             <$> with sess (getFromSession "ThentosSessionData")

-- | This is not race-condition-free, but the session only lives in
-- the single thread that handles the single request, so thread-safety
-- is not required.  (FIXME: think about this some more.)
modifySessionData :: (FrontendSessionData -> (FrontendSessionData, a)) -> FH a
modifySessionData op = do
    sessionData <- getSessionData
    with sess $ do
        let (sessionData', val) = op sessionData
        setInSession "ThentosSessionData" . cs . Aeson.encode $ sessionData'
        commitSession
        return val

-- | A version of 'modifySessionData' without return value.
modifySessionData' :: (FrontendSessionData -> FrontendSessionData) -> FH ()
modifySessionData' f = modifySessionData $ (, ()) . f

-- | Construct the service login state (extract service id from
-- params, callback is passed as argument; crash if argument is
-- Nothing).  Write it to the snap session and return it.
setServiceLoginState :: FH ServiceLoginState
setServiceLoginState = do
    sid <- getParam "sid" >>= maybe
             (crash 400 "Service login: missing Service ID.")
             (return . ServiceId . cs)
    uri <- getsRequest rqURI >>= \ callbackST -> either
             (\ msg -> crash 400 $ "Service login: malformed redirect URI: " <> cs (show (msg, callbackST)))
             (return)
             (parseURI laxURIParserOptions $ cs callbackST)

    let val = ServiceLoginState sid uri
    modifySessionData' $ fsdServiceLoginState .~ Just val
    logger DEBUG ("setServiceLoginState: set to " <> show val)
    return val

-- | Recover the service login state from snap session, remove it
-- there, and return it.  If no service login state is stored, return
-- 'Nothing'.
popServiceLoginState :: FH (Maybe ServiceLoginState)
popServiceLoginState = modifySessionData $
    \ fsd -> (fsdServiceLoginState .~ Nothing $ fsd, fsd ^. fsdServiceLoginState)

-- | Recover the service login state from snap session like
-- 'popServiceLoginState', but do not remove it.
getServiceLoginState :: FH (Maybe ServiceLoginState)
getServiceLoginState = modifySessionData $ \ fsd -> (fsd, fsd ^. fsdServiceLoginState)

sendFrontendMsgs :: [FrontendMsg] -> FH ()
sendFrontendMsgs msgs = modifySessionData' $ fsdMessages %~ (++ msgs)

sendFrontendMsg :: FrontendMsg -> FH ()
sendFrontendMsg = sendFrontendMsgs . (:[])

popAllFrontendMsgs :: FH [FrontendMsg]
popAllFrontendMsgs = modifySessionData $ \ fsd -> (fsdMessages .~ [] $ fsd, fsd ^. fsdMessages)


-- * error handling

crash' :: (Show a) => Int -> a -> SBS -> Handler b v x
crash' status logMsg usrMsg = do
    logger DEBUG $ show (status, logMsg, usrMsg)
    modifyResponse $ setResponseStatus status usrMsg
    blaze . errorPage . cs $ usrMsg
    getResponse >>= finishWith

crash :: Int -> SBS -> Handler b v x
crash status usrMsg = crash' status () usrMsg

-- | Use this for internal errors.  Ideally, we shouldn't have to even
-- write those, but structure the code in a way that makes places
-- where this is called syntactially unreachable.  Oh well.
crash500 :: (Show a) => a -> Handler b v x
crash500 a = do
    logger CRITICAL $ show ("*** internal error: " <> show a)
    crash 500 "internal error.  we are very sorry."

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
_tweakURI parse serialize tweak uriBS = either er ok $ parse laxURIParserOptions uriBS
  where
    ok = return . cs . toLazyByteString . serialize . tweak
    er = crash500 . ("_tweakURI" :: ST, uriBS,)


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

    when (loggingEnabled conf) . liftIO . hPutStrLn stderr $ ST.unpack msgs
    _ <- try (serve site)
         :: IO (Either SomeException ())
    doCleanup
  where
    loggingEnabled = not . (== Just False) . getVerbose

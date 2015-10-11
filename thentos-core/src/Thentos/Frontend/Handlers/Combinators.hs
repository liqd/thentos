{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}

module Thentos.Frontend.Handlers.Combinators where

import Data.ByteString.Builder (toLazyByteString)
import Control.Applicative (pure)
import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.), (%~), (.~))
import Control.Monad.Except (liftIO, throwError)
import Control.Monad.State.Class (gets)
import "cryptonite" Crypto.Random (ChaChaDRG)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (SBS, ST, cs)
import Data.Text.Encoding (encodeUtf8)
import Data.Void (Void)
import LIO.Error (AnyLabelError)
import System.Log.Missing (logger)
import System.Log (Priority(DEBUG, CRITICAL, INFO))
import Text.Digestive.Form (Form)
import Text.Digestive.View (View)
import Servant.Server
import URI.ByteString
    ( parseURI, parseRelativeRef, laxURIParserOptions, serializeURI, serializeRelativeRef
    , URI(..), RelativeRef(..), URIParserOptions, Query(..)
    )

import qualified Data.Aeson as Aeson
import qualified Text.Blaze.Html5 as H

import LIO.Missing
import Thentos.Action
import Thentos.Action.Core
import Thentos.Config
import Thentos.Frontend.Pages
import Thentos.Frontend.Types
import Thentos.Types
import Thentos.Util


-- * dashboard construction

-- | Call 'buildDashboard' to consruct a dashboard page and render it
-- into the application monad.
renderDashboard :: DashboardTab -> (User -> [Role] -> H.Html) -> FH H.Html
renderDashboard tab pagelet = buildDashboard tab pagelet

-- | Like 'renderDashboard', but take a pagelet builder instead of a
-- pagelet.
renderDashboard' :: DashboardTab -> (User -> [Role] -> H.Html) -> FH H.Html
renderDashboard' tab pagelet = buildDashboard tab pagelet

-- | Take a dashboard tab and a pagelet, and consruct the dashboard
-- page.
buildDashboard :: DashboardTab -> (User -> [Role] -> H.Html) -> FH H.Html
buildDashboard tab pagelet = buildDashboard' tab (\ u -> pure . pagelet u)

-- | Like 'buildDashboard', but take a pagelet builder instead of a
-- pagelet.
buildDashboard' :: DashboardTab -> (User -> [Role] -> FH H.Html) -> FH H.Html
buildDashboard' tab pageletBuilder = do
    runAsUser $ \ _ sessionLoginData -> do
        msgs <- popAllFrontendMsgs
        let uid = sessionLoginData ^. fslUserId
        (_, user) <- snapRunAction $ lookupConfirmedUser uid
        roles     <- snapRunAction $ agentRoles (UserA uid)
        dashboardPagelet msgs roles tab <$> pageletBuilder user roles


{-
-- * form rendering and processing

-- | Take a form, a pagelet matching this form, a dashboard tab to render it in, and an action to be
-- performed on the form data once submitted.  Depending on the 'runForm' result, either render the
-- form or process it calling the action.  (The formAction passed to 'runForm' is the URI of the
-- current request.)
runPageletForm :: forall v a .
       Form v FH a  -- ^ result constructor
    -> (ST -> View v -> User -> [Role] -> H.Html)  -- ^ dashboard tab contents
    -> DashboardTab  -- ^ dashboard tab identifier
    -> (a -> FH ())  -- ^ result consumer
    -> FH ()
runPageletForm f pagelet = runPageletForm' f (\ formAction v u -> return . pagelet formAction v u)

-- | Like 'runPageletForm', but takes a page builder instead of a page (this is more for internal
-- use).
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
runPageForm f page a = do
    runPageForm' f (\ formAction -> return . page formAction) a

-- | Full-page version of 'runPageletForm''.
runPageForm' :: forall v a .
       Form v FH a
    -> (ST -> View v -> FH H.Html)
    -> (a -> FH ())
    -> FH ()
runPageForm' f buildPage = runHandlerForm f handler
  where
    handler :: ST -> View v -> FH H.Html
    handler formAction view = buildPage formAction view

-- | Version of of 'runPageletForm'' that takes a handler rather than a pagelet, and calls that in
-- order to render the empty form.  (For symmetry, the function name could be primed, but there is
-- no non-monadic way to call a handler, so there is only one version of @runHandlerForm@.)
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

-}



-- * authentication

-- | Call 'runAsUserOrNot', and redirect to login page if not logged
-- in.
runAsUser :: (FrontendSessionData -> FrontendSessionLoginData -> FH a) -> FH a
runAsUser = (`runAsUserOrNot` redirectSBS "/user/login")

-- | Runs a given handler with the credentials and the session data of
-- the currently logged-in user.  If not logged in, call a default
-- handler that runs without any special clearance.
-- We don't have to verify that the user matches the session, since both are
-- stored in encrypted in the session cookie, so they cannot be manipulated
-- by the user.
runAsUserOrNot :: (FrontendSessionData -> FrontendSessionLoginData -> FH a) -> FH a -> FH a
runAsUserOrNot loggedInHandler loggedOutHandler = do
    sessionData :: FrontendSessionData <- getSessionData
    case sessionData ^. fsdLogin of
        Just sessionLoginData -> do
            loggedInHandler sessionData sessionLoginData
        Nothing -> loggedOutHandler

popAllFrontendMsgs = undefined
snapRunAction = undefined
getSessionData = undefined




{-

-- * session management

-- | Extract 'FrontendSessionData' object from cookie.  If n/a, return
-- an empty one.  If a value is stored, but cannot be decoded, crash.
getSessionData :: FH FrontendSessionData
getSessionData = fromMaybe emptyFrontendSessionData
               . (>>= Aeson.decode . cs)
             <$> with sess (getFromSession "ThentosSessionData")

-- | Only store data that doesn't change within a session
-- (e.g. the session token, user id) in the session cookie to avoid
-- race conditions that might lose changes between requests.
--
-- FIXME: move service login state, msg queue from FrontendSessionData
-- to DB.
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
    rrf <- getsRequest rqURI >>= \ callbackSBS -> either
             (\ msg -> crash 400 $ "Service login: malformed redirect URI: " <> cs (show (msg, callbackSBS)))
             return
             (parseRelativeRef laxURIParserOptions callbackSBS)

    let val = ServiceLoginState sid rrf
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
crash status = crash' status ()

-- | Use this for internal errors.  Ideally, we shouldn't have to even
-- write those, but structure the code in a way that makes places
-- where this is called syntactially unreachable.  Oh well.
crash500 :: (Show a) => a -> Handler b v x
crash500 a = do
    logger CRITICAL $ show ("*** internal error: " <> show a)
    crash 500 "internal error.  we are very sorry."

permissionDenied :: AnyLabelError -> Handler b v x
permissionDenied labelError = do
    logger INFO $ "Label error :" ++ show labelError
    modifyResponse $ setResponseStatus 403 "Permission denied"
    blaze permissionDeniedPage
    getResponse >>= finishWith



-- | This function handles particular error cases for the frontend and propagates others in 'Left'
-- values.
snapHandleSomeErrors :: Either (ActionError Void) a -> FH (Either (ActionError Void) a)
snapHandleSomeErrors (Right v) = return $ Right v
snapHandleSomeErrors (Left e) = case e of
    ActionErrorAnyLabel labelError -> permissionDenied labelError
    _ -> return $ Left e






-- * uri manipulation

urlConfirm :: HttpConfig -> ST -> ST -> ST
urlConfirm feConfig path token = exposeUrl feConfig <//> toST ref
  where
    ref   = RelativeRef Nothing (cs path) (Query query) Nothing
    query = [("token", urlEncode . encodeUtf8 $ token)]
    toST  = cs . toLazyByteString . serializeRelativeRef

-}

redirectRR :: RelativeRef -> FH a
redirectRR = throwError . OtherError . FrontendErrorRedirectRR

redirectURI :: URI -> FH ()
redirectURI = throwError . OtherError . FrontendErrorRedirectURI

{-




{-


-- | Wraps the top-level handler to prevent cross-site request forgery
csrfify :: FH () -> FH ()
csrfify handler = do
    mSessData <- with sess (getFromSession "ThentosSessionData")
    case mSessData of
        -- FIXME: sensible error handling. Opinions vary on what to tell the
        -- user in this case. For discussion, see e.g.
        -- http://security.stackexchange.com/questions/8446/how-should-a-web-page-respond-to-a-csrf-attack
        Just _ -> blanketCSRF sess (crash 500 "csrf badness") handler
        Nothing -> handler





{-# LANGUAGE OverloadedStrings #-}

-- | The contents of this module are copied from the snap-extras package to
-- avoid several large dependencies of snap-extras that we're not using at all.
module Snap.Inline (blanketCSRF, handleCSRF) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding    as T
import           Snap
import           Snap.Snaplet.Session

------------------------------------------------------------------------------
-- | Use this function to wrap your whole site with CSRF protection.  Due to
-- security considerations, the way Snap parses file uploads
-- means that the CSRF token cannot be checked before the file uploads have
-- been handled.  This function protects your whole site except for handlers
-- of multipart/form-data forms (forms with file uploads).  To protect those
-- handlers, you have to call handleCSRF explicitly after the file has been
-- processed.
blanketCSRF :: SnapletLens v SessionManager
            -- ^ Lens to the session snaplet
            -> Handler b v ()
            -- ^ Handler to run if the CSRF check fails
            -> Handler b v ()
            -- ^ Handler to let through when successful.
            -> Handler b v ()
blanketCSRF session onFailure onSucc = do
    h <- getHeader "content-type" `fmap` getRequest
    if maybe False (B.isInfixOf "multipart/form-data") h
      then onSucc
      else handleCSRF session onFailure onSucc


------------------------------------------------------------------------------
-- | If a request is a POST, check the CSRF token and fail with the specified
-- handler if the check fails.  If if the token is correct or if it's not a
-- POST request, then control passes through as a no-op.
handleCSRF :: SnapletLens v SessionManager
           -- ^ Lens to the session snaplet
           -> Handler b v ()
           -- ^ Handler to run on failure
           -> Handler b v ()
           -- ^ Handler to let through when successful.
           -> Handler b v ()
handleCSRF session onFailure onSucc = do
    m <- getsRequest rqMethod
    if m /= POST
      then onSucc
      else do
        tok <- getParam "_csrf"
        realTok <- with session csrfToken
        if tok == Just (T.encodeUtf8 realTok)
          then onSucc
          else onFailure >> getResponse >>= finishWith



-
--- | Write some 'Html' as response.  We use pretty printing instead of
--- 'Text.Blaze.Html.Renderer.Utf8.renderHtml'.  This may even be a good idea in production, because
--- it makes things more transparent while it is not clear how much performance improvement can come
--- from killing all the redundant whitespace.  (If we only didn't have to go through 'String'..)
---
--- (Missing in "Snap.Blaze"; package snap-blaze.)
---
--- No pull request has been made for this.  snap-blaze contains nothing beyond this function, and we
--- can afford inlining it.
-blaze :: MonadSnap m => Html -> m ()
-blaze response = do
-    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
-    writeText . cs $ Text.Blaze.Html.Renderer.Pretty.renderHtml response





-}
-}

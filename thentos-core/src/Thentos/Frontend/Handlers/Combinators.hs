{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}

module Thentos.Frontend.Handlers.Combinators where

import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.), (%~), (.~))
import Control.Monad.Except (liftIO, throwError, catchError)
import Control.Monad.State.Class (get, gets, modify, state)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (left)
import "cryptonite" Crypto.Random (ChaChaDRG)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (SBS, ST, cs)
import Data.Text.Encoding (encodeUtf8)
import Data.Void (Void)
import LIO.Error (AnyLabelError)
import Network.HTTP.Types (urlEncode)
import Servant hiding (URI)
import System.Log.Missing (logger)
import System.Log (Priority(DEBUG, CRITICAL, INFO))
import Text.Digestive.Form (Form)
import Text.Digestive.Snap (runForm)
import Text.Digestive.View (View)
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
import Thentos.Frontend.State
import Thentos.Frontend.Types
import Thentos.Types

import qualified Thentos.Action.Unsafe as U
import qualified Thentos.Action.SimpleAuth as U


{-
import Snap.Core
    ( getResponse, finishWith, urlEncode, getParam
    , rqURI, getsRequest, redirect', modifyResponse, setResponseStatus
    )
import Snap.Inline (blanketCSRF)
import Snap.Snaplet (Handler, with)
import Snap.Snaplet.Session (commitSession, setInSession, getFromSession, csrfToken)
-- import Snap.Missing (blaze)
-}


-- * dashboard construction

-- | Call 'buildDashboard' to consruct a dashboard page and render it into the application monad.
renderDashboard :: DashboardTab -> (User -> [Role] -> H.Html) -> FAction H.Html
renderDashboard tab pagelet = renderDashboard' tab (\u -> return . pagelet u)

-- | Like 'buildDashboard', but take a pagelet builder instead of a pagelet.
renderDashboard' :: DashboardTab -> (User -> [Role] -> FAction H.Html) -> FAction H.Html
renderDashboard' tab pageletBuilder = do
    runAsUserOrLogin $ \_ sessionLoginData -> do
        msgs <- popAllFrontendMsgs
        let uid = sessionLoginData ^. fslUserId
        (_, user) <- lift $ lookupConfirmedUser uid
        roles     <- lift $ agentRoles (UserA uid)
        dashboardPagelet msgs roles tab <$> pageletBuilder user roles


-- * form rendering and processing

{-
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
runPageletForm f pagelet = runPageletForm' f (\formAction v u -> return . pagelet formAction v u)

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
    -> (ST -> ST -> View v -> H.Html)
    -> (a -> FH ())
    -> FH ()
runPageForm f page a = do
    tok <- with sess csrfToken
    runPageForm' f (\formAction -> return . page tok formAction) a

-- | Full-page version of 'runPageletForm''.
runPageForm' :: forall v a .
       Form v FH a
    -> (ST -> View v -> FH H.Html)
    -> (a -> FH ())
    -> FH ()
runPageForm' f buildPage = runHandlerForm f handler
  where
    handler :: ST -> View v -> FH ()
    handler formAction view = buildPage formAction view >>= blaze

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

-- | Call 'runAsUser', and redirect to login page if not logged in.
runAsUserOrLogin :: (FrontendSessionData -> FrontendSessionLoginData -> FAction a) -> FAction a
runAsUserOrLogin = (`runAsUser` redirect' "/user/login")

-- | Runs a given handler with the credentials and the session data of the currently logged-in user.
-- If not logged in, call a default handler that runs without any special clearance.
runAsUser :: (FrontendSessionData -> FrontendSessionLoginData -> FAction a)
      -> FAction a -> FAction a
runAsUser loggedInHandler loggedOutHandler = do
    sessionData :: FrontendSessionData <- get
    case sessionData ^. fsdLogin of
        Just sessionLoginData@(FrontendSessionLoginData tok uid _)  -> do
            lift $ accessRightsByAgent'P (UserA uid) >>= grantAccessRights'P
            loggedInHandler sessionData sessionLoginData
        Nothing -> loggedOutHandler


-- * session management

{-
-- | Construct the service login state (extract service id from
-- params, callback is passed as argument; crash if argument is
-- Nothing).  Write it to the snap session and return it.
setServiceLoginState :: FH ServiceLoginState
setServiceLoginState = do
    sid <- getParam "sid" >>= maybe
             (crash $ FActionError500 "Service login: missing Service ID.")
             (return . ServiceId . cs)
    rrf <- getsRequest rqURI >>= \callbackSBS -> either
             (\msg -> crash . FActionError500 $ "Service login: malformed redirect URI: " ++ show (msg, callbackSBS))
             return
             (parseRelativeRef laxURIParserOptions callbackSBS)

    let val = ServiceLoginState sid rrf
    modifySessionData' $ fsdServiceLoginState .~ Just val
    logger DEBUG ("setServiceLoginState: set to " <> show val)
    return val
-}

-- | Recover the service login state from snap session, remove it
-- there, and return it.  If no service login state is stored, return
-- 'Nothing'.
popServiceLoginState :: FAction (Maybe ServiceLoginState)
popServiceLoginState = state $
    \fsd -> (fsd ^. fsdServiceLoginState, fsdServiceLoginState .~ Nothing $ fsd)

-- | Recover the service login state from snap session like
-- 'popServiceLoginState', but do not remove it.
getServiceLoginState :: FAction (Maybe ServiceLoginState)
getServiceLoginState = gets (^. fsdServiceLoginState)

sendFrontendMsgs :: [FrontendMsg] -> FAction ()
sendFrontendMsgs msgs = modify $ fsdMessages %~ (++ msgs)

sendFrontendMsg :: FrontendMsg -> FAction ()
sendFrontendMsg = sendFrontendMsgs . (:[])

popAllFrontendMsgs :: FAction [FrontendMsg]
popAllFrontendMsgs = state $ \s -> (s ^. fsdMessages, fsdMessages .~ [] $ s)


-- * uri manipulation

emailConfirmUrl :: HttpConfig -> ST -> ST -> ST
emailConfirmUrl feConfig path token = exposeUrl feConfig <//> toST ref
  where
    ref   = RelativeRef Nothing (cs path) (Query query) Nothing
    query = [("token", urlEncode False . encodeUtf8 $ token)]
    toST  = cs . toLazyByteString . serializeRelativeRef

redirect' :: SBS -> FAction a
redirect' = throwError . OtherError . FActionError303

redirectURI :: URI -> FAction a
redirectURI ref = redirect' (cs . toLazyByteString . serializeURI $ ref)

redirectRR :: RelativeRef -> FAction a
redirectRR ref = redirect' (cs . toLazyByteString . serializeRelativeRef $ ref)


{-
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
    er = crash $ FActionError500 . ("_tweakURI" :: ST, uriBS,)


-- * actions vs. snap

-- | Like 'snapRunActionE', but sends a snap error response in case of error rather than returning a
-- left value.
snapRunAction :: Action Void a -> FH a
snapRunAction = (>>= snapHandleAllErrors) . snapRunActionE

snapRunAction'P :: Action Void a -> FH a
snapRunAction'P = (>>= snapHandleAllErrors) . snapRunActionE'P

-- | Call 'snapHandleSomeErrors', and if error is still unhandled, call 'crash500'.
snapHandleAllErrors :: Either (ActionError Void) a -> FH a
snapHandleAllErrors eth = do
    result <- snapHandleSomeErrors eth
    case result of
        Right val -> return val
        Left err -> crash500 err

getActionState :: FH ActionState
getActionState = do
    pool <- gets (^. connPool)
    rn :: MVar ChaChaDRG <- gets (^. rng)
    cf :: ThentosConfig  <- gets (^. cfg)
    return $ ActionState (pool, rn, cf)

-- | Run action with the clearance derived from thentos session token.
snapRunActionE :: Action Void a -> FH (Either (ActionError Void) a)
snapRunActionE action = do
    st :: ActionState         <- getActionState
    fs :: FrontendSessionData <- getSessionData

    result <- case (^. fslToken) <$> fs ^. fsdLogin of
        Just tok -> liftIO $ runActionInThentosSessionE tok st action
        Nothing  -> liftIO $ runActionE                     st action
    snapHandleSomeErrors result

-- | Call action with top clearance.
snapRunActionE'P :: Action Void a -> FH (Either (ActionError Void) a)
snapRunActionE'P action = do
    st :: ActionState <- getActionState
    result <- liftIO $ runActionWithClearanceE dcTop st action
    snapHandleSomeErrors result

-- | This function handles particular error cases for the frontend and propagates others in 'Left'
-- values.
snapHandleSomeErrors :: Either (ActionError Void) a -> FH (Either (ActionError Void) a)
snapHandleSomeErrors (Right v) = return $ Right v
snapHandleSomeErrors (Left e) = case e of
    ActionErrorAnyLabel labelError -> permissionDenied labelError
    _ -> return $ Left e

-- | Wraps the top-level handler to prevent cross-site request forgery
csrfify :: FH () -> FH ()
csrfify handler = do
    mSessData <- with sess (getFromSession "ThentosSessionData")
    case mSessData of
        -- FIXME: sensible error handling. Opinions vary on what to tell the
        -- user in this case. For discussion, see e.g.
        -- http://security.stackexchange.com/questions/8446/how-should-a-web-page-respond-to-a-csrf-attack
        Just _ -> blanketCSRF sess (crash $ FActionError500 "csrf badness") handler
        Nothing -> handler
-}

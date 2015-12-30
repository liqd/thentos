{-# LANGUAGE OverloadedStrings      #-}

module Thentos.Frontend.Handlers.Combinators where

import Control.Lens ((^.), (%~), (.~), _Just)
import Control.Monad.State.Class (get, gets, modify, state)
import Data.ByteString.Builder (toLazyByteString)
import Data.String.Conversions (SBS, ST, cs)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (urlEncode)
import System.Log (Priority(DEBUG))
import Text.Digestive.View (View)
import URI.ByteString (serializeURI, serializeRelativeRef, URI(..), RelativeRef(..), Query(..))

import qualified Text.Blaze.Html5 as H

import Thentos.Action
import Thentos.Config
import Thentos.Frontend.Pages
import Thentos.Frontend.State (crash)
import Thentos.Frontend.Types
import Thentos.Types

import qualified Thentos.Action.Unsafe as U
import qualified Thentos.Action.SimpleAuth as U


-- * helpers

-- FIXME: these need to be replaced.

liftU :: U.UnsafeAction FActionError FrontendSessionData a -> FAction a
liftU = U.unsafeAction

loggerF :: (Show v) => v -> FAction ()
loggerF = U.unsafeAction . loggerU

loggerU :: (Show v) => v -> U.UnsafeAction FActionError s ()
loggerU = U.logger System.Log.DEBUG . show


-- * dashboard construction

-- | If logged in: set current dashboard tab.
setTab :: DashboardTab -> FAction ()
setTab = modify . (fsdLogin . _Just . fslDashboardTab .~) . Just

-- | Call 'renderDashboard'' to construct a dashboard page and render it in the frontend monad.
renderDashboard :: (User -> [Group] -> H.Html) -> FAction H.Html
renderDashboard pagelet = renderDashboard' (\u -> return . pagelet u)

-- | Like 'renderDashboard', but take a pagelet builder instead of a pagelet.
renderDashboard' :: (User -> [Group] -> FAction H.Html) -> FAction H.Html
renderDashboard' pageletBuilder = do
    runAsUserOrLogin $ \fsd sessionLoginData -> do
        (uid, user) <- lookupConfirmedUser (sessionLoginData ^. fslUserId)
        roles       <- agentRoles (UserA uid)
        clearAllFrontendMsgs
        dashboardPagelet fsd roles <$> pageletBuilder user roles


-- * authentication

-- | Call 'runAsUser', and redirect to login page if not logged in.
runAsUserOrLogin :: (FrontendSessionData -> FrontendSessionLoginData -> FAction a) -> FAction a
runAsUserOrLogin = (`runAsUser` redirect' "/user/login")

-- | Runs a given handler with the credentials and the session data of the currently logged-in user.
-- If not logged in, call a default handler that runs without any special clearance.  (NOTE:
-- Clearance modification does not happen here, but in 'enterFAction'.)
runAsUser :: (FrontendSessionData -> FrontendSessionLoginData -> FAction a)
      -> FAction a -> FAction a
runAsUser loggedInHandler loggedOutHandler = do
    sessionData <- get
    case sessionData ^. fsdLogin of
        Just sessionLoginData -> loggedInHandler sessionData sessionLoginData
        Nothing               -> loggedOutHandler


-- * session management

-- | Recover the service login state from session, remove it there, and return it.  If no service
-- login state is stored, return 'Nothing'.
popServiceLoginState :: FAction (Maybe ServiceLoginState)
popServiceLoginState = state $
    \fsd -> (fsd ^. fsdServiceLoginState, fsdServiceLoginState .~ Nothing $ fsd)

getServiceLoginState :: FAction ServiceLoginState
getServiceLoginState = gets (^. fsdServiceLoginState) >>= maybe err pure
  where
    err = crash $ FActionError500 "Service login: no state."

sendFrontendMsgs :: [FrontendMsg] -> FAction ()
sendFrontendMsgs msgs = do
    loggerF msgs
    modify $ fsdMessages %~ (++ msgs)

sendFrontendMsg :: FrontendMsg -> FAction ()
sendFrontendMsg = sendFrontendMsgs . (:[])

clearAllFrontendMsgs :: FAction ()
clearAllFrontendMsgs = state $ \s -> ((), fsdMessages .~ [] $ s)

showPageWithMessages :: (FrontendSessionData -> View H.Html -> ST -> H.Html)
    -> View H.Html -> ST -> FAction H.Html
showPageWithMessages page v a = do
    html <- (\fsd -> page fsd v a) <$> get
    clearAllFrontendMsgs
    return html


-- * uri manipulation

emailConfirmUrl :: HttpConfig -> ST -> ST -> ST
emailConfirmUrl feConfig path token = exposeUrl feConfig <//> toST ref
  where
    ref   = RelativeRef Nothing (cs path) (Query query) Nothing
    query = [("token", urlEncode False . encodeUtf8 $ token)]
    toST  = cs . toLazyByteString . serializeRelativeRef

redirect' :: SBS -> FAction a
redirect' = crash . FActionError303

redirectURI :: URI -> FAction a
redirectURI = redirect' . cs . toLazyByteString . serializeURI

redirectRR :: RelativeRef -> FAction a
redirectRR = redirect' . cs . toLazyByteString . serializeRelativeRef

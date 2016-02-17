{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}

module Thentos.Frontend.Handlers.Combinators where

import Data.ByteString.Builder (toLazyByteString)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (urlEncode)
import Servant (Get, Post, ServerT)
import Text.Digestive.Form (Form, text, (.:))
import Text.Digestive.View (View)
import URI.ByteString (serializeURI, serializeRelativeRef, URI(..), RelativeRef(..), Query(..))

import qualified Servant.Missing as UnprotectedFormH
import qualified Text.Blaze.Html5 as H

import Thentos.Action
import Thentos.Action.TCB
import Thentos.Config
import Thentos.Ends.Types
import Thentos.Frontend.CSRF
import Thentos.Frontend.Pages
import Thentos.Frontend.Types
import Thentos.Prelude
import Thentos.Types

-- * types

type GetH  = Get  '[HTM] H.Html
type PostH = Post '[HTM] H.Html

-- All Post request should go through FormH (which handles both Get and Post) such that common
-- verification such as CSRF protection is enabled.
type FormH a = UnprotectedFormH.FormH HTM H.Html a


-- * form construction

formH :: MonadFAction m
  => ST                               -- ^ formAction
  -> Form H.Html m payload            -- ^ processor1
  -> (payload -> m H.Html)            -- ^ processor2
  -> (View H.Html -> ST -> m H.Html)  -- ^ renderer
  -> ServerT (FormH payload) m
formH fa p1 p2 =
    UnprotectedFormH.formH fa
        ((,) <$> (CsrfToken <$> ("_csrf" .: text Nothing)) <*> p1)
        (\(csrfToken, payload)-> checkCsrfToken csrfToken >> p2 payload)

unprotectedFormH :: MonadFAction m
  => ST                               -- ^ formAction
  -> Form H.Html m payload            -- ^ processor1
  -> (payload -> m H.Html)            -- ^ processor2
  -> (View H.Html -> ST -> m H.Html)  -- ^ renderer
  -> ServerT (FormH payload) m
unprotectedFormH = UnprotectedFormH.formH

-- * dashboard construction

-- | If logged in: set current dashboard tab.
setTab :: DashboardTab -> FAction ()
setTab = (fsdLogin . _Just . fslDashboardTab ?=)

-- | Call 'renderDashboard'' to construct a dashboard page and render it in the frontend monad.
renderDashboard :: (User -> [Group] -> H.Html) -> FAction H.Html
renderDashboard pagelet = renderDashboard' (\u -> return . pagelet u)

-- | Like 'renderDashboard', but take a pagelet builder instead of a pagelet.
renderDashboard' :: (User -> [Group] -> FAction H.Html) -> FAction H.Html
renderDashboard' pageletBuilder = do
    runAsUserOrLogin $ \fsd sessionLoginData -> do
        (uid, user) <- lookupConfirmedUser (sessionLoginData ^. fslUserId)
        groups      <- agentGroups (UserA uid)
        clearAllFrontendMsgs
        dashboardPagelet fsd groups <$> pageletBuilder user groups


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
getServiceLoginState = use fsdServiceLoginState >>= maybe err pure
  where
    err = crash $ FActionError500 "Service login: no state."

sendFrontendMsgs :: [FrontendMsg] -> FAction ()
sendFrontendMsgs msgs = do
    loggerD msgs
    fsdMessages %= (++ msgs)

sendFrontendMsg :: FrontendMsg -> FAction ()
sendFrontendMsg = sendFrontendMsgs . (:[])

clearAllFrontendMsgs :: FAction ()
clearAllFrontendMsgs = state $ \s -> ((), s & fsdMessages .~ [])

showPageWithMessages :: (FrontendSessionData -> View H.Html -> ST -> H.Html)
    -> View H.Html -> ST -> FAction H.Html
showPageWithMessages page v a = do
    fsd <- get
    clearAllFrontendMsgs
    return $ page fsd v a


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

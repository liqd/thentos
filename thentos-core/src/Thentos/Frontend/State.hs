{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Thentos.Frontend.State
    ( enterFAction
    , serveFAction
    , getFrontendCfg
    , fActionServantErr
    )
where

import Control.Monad.Except.Missing (finally)
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Data.Configifier (Tagged(Tagged))
import LIO.TCB (ioTCB)
import Network.Wai (Middleware, Application)
import Network.Wai.Session (SessionStore, Session, withSession)
import Servant (ServantErr, (:>), serve, HasServer, ServerT, Server, (:~>)(Nat), unNat)
import Servant.Server (errHTTPCode, errHeaders, errBody, err303, err404, err400, err500)
import Servant.Server.Internal.Enter (Enter, enter)
import Servant.Session (SSession)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Web.Cookie (SetCookie, def, setCookieName)

import qualified Data.ByteString as SBS
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai.Session.Map as SessionMap

import Thentos.Prelude
import Thentos.Action.Core
import Thentos.Action.Types
import Thentos.Action.TCB
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Frontend.CSRF
import Thentos.Frontend.Types
import Thentos.Types

import qualified Thentos.Frontend.Pages.Core as Pages


-- BUG #406: EU-required user notifications about cookies
-- FUTUREWORK: more efficient refresh (only if changed or after 20% of the age has been passed)


-- * errors

fActionServantErr :: ActionError FActionError -> IO ServantErr
fActionServantErr = errorInfoToServantErr mkServErr .
                                    actionErrorInfo (thentosErrorInfo f)
  where
    f :: FActionError -> (Maybe (Priority, String), ServantErr, ST)
    f (FActionError303 uri) =
        (Nothing, err303 { errHeaders = [("Location", uri)] }, "redirect: " <> cs uri)
    f FActionError404 =
        (Nothing, err404, "page not found.")
    f e@FActionErrorNoToken =
        (Just (DEBUG, show e), err400, "email confirmation url broken: no token.")
    f e@FActionErrorCreateService =
        (Just (DEBUG, show e), err400, "could not create service.")
    f e@FActionErrorServiceLoginNoCbUrl =
        (Just (DEBUG, show e), err400, "no or broken callback url.")
    f e@(FActionError500 _) =
        (Just (ERROR, show e), err500, "we are very sorry.")

    mkServErr :: ServantErr -> ST -> ServantErr
    mkServErr baseErr msg = baseErr
        { errBody = cs . renderHtml $ makeErrorPage (errHTTPCode baseErr) msg
        , errHeaders = ("Content-Type", "text/html; charset=utf-8") : errHeaders baseErr
        }

    makeErrorPage :: Int -> ST -> Html
    makeErrorPage 403 = const Pages.permissionDeniedPage
    makeErrorPage 404 = const Pages.notFoundPage
    makeErrorPage _   = Pages.errorPage . cs


-- * middleware

type FSession        fsd = Session IO () fsd
type FSessionMap     fsd = Vault.Key (FSession fsd) -> Maybe (FSession fsd)
type FSessionStore   fsd = SessionStore IO () fsd
type FServantSession fsd = SSession IO () fsd
type FSessionKey     fsd = Vault.Key (FSession fsd)

thentosSetCookie :: SetCookie
thentosSetCookie = def { setCookieName = "thentos" }  -- FIXME: make 'SetCookie' configurable with configifier.

cookieName :: SetCookie -> SBS
cookieName setCookie =
    if cookieNameValid n
        then n
        else error $ "Thentos.Frontend.State: bad cookie name: " ++ show n
  where
    n = setCookieName setCookie

cookieNameValid :: SBS -> Bool
cookieNameValid = SBS.all (`elem` (fromIntegral . ord <$> '_':['a'..'z']))

sessionMiddleware :: Proxy fsd -> SetCookie -> IO (Middleware, FSessionKey fsd)
sessionMiddleware Proxy setCookie = do
    smap :: FSessionStore fsd <- SessionMap.mapStore_
    key  :: Vault.Key (FSession fsd) <- Vault.newKey
    return (withSession smap (cookieName setCookie) setCookie key, key)


-- * frontend action monad

type FActionStack = ActionStack FActionError FrontendSessionData

-- FIXME: Using MonadFAction instead of FActionStack leads to an ambiguity here.
serveFAction :: forall api.
        ( HasServer api
        , Enter (ServerT api FActionStack) (FActionStack :~> ExceptT ServantErr IO) (Server api)
        )
     => Proxy api -> ServerT api FActionStack -> ActionEnv -> IO Application
serveFAction _ fServer aState =
    app <$> sessionMiddleware (Proxy :: Proxy FrontendSessionData) thentosSetCookie
  where
    app :: (Middleware, FSessionKey FrontendSessionData) -> Application
    app (mw, key) = mw $ serve (Proxy :: Proxy (FServantSession FrontendSessionData :> api)) (server' key)

    server' :: FSessionKey FrontendSessionData -> FSessionMap FrontendSessionData -> Server api
    server' key smap = enter nt fServer
      where
        run :: FActionStack a -> ExceptT ServantErr IO a
        run = ExceptT . (>>= _Left fActionServantErr) . (fst <$>)
            . runActionE emptyFrontendSessionData aState
        nt :: FActionStack :~> ExceptT ServantErr IO
        nt = enterFAction key smap extendClearanceOnThentosSession (Nat run)

type ExtendClearanceOnSessionToken m = ThentosSessionToken -> m ()

enterFAction
    :: ( MonadRandom m, MonadThentosIO m, MonadError500 err m, MonadHasSessionCsrfToken s m
       , MonadViewCsrfSecret env m, MonadUseThentosSessionToken s m)
    => FSessionKey s
    -> FSessionMap s
    -> ExtendClearanceOnSessionToken m
    -> m :~> ExceptT ServantErr IO
    -> m :~> ExceptT ServantErr IO
enterFAction key smap extendClearanceOnSessionToken nat = Nat $ \fServer -> unNat nat $ do
    case smap key of
        Nothing ->
            -- FIXME: this case should not be code 500, as it can (probably) be provoked by
            -- the client.
            throwError500 "Could not read cookie."
        Just (lkup, ins) -> do
            cookieToFSession (lkup ())
            maybeSessionToken <- use getThentosSessionToken
            -- Update privileges and refresh the CSRF token if there is a session token
            mapM_ extendClearanceOnSessionToken maybeSessionToken
            when (isJust maybeSessionToken) refreshCsrfToken
            fServer `finally` (do
                clearCsrfToken -- could be replaced by 'refreshCsrfToken'
                cookieFromFSession (ins ()))

-- | Write 'FrontendSessionData' from the servant-session state to 'MonadFAction' state.  If there
-- is no state, do nothing.
cookieToFSession :: (MonadThentosIO m, MonadState s m) => IO (Maybe s) -> m ()
cookieToFSession r = liftLIO (ioTCB r) >>= mapM_ put

-- | Read 'FrontendSessionData' from 'MonadFAction' and write back into servant-session state.
cookieFromFSession :: (MonadThentosIO m, MonadState s m) => (s -> IO ()) -> m ()
cookieFromFSession w = get >>= liftLIO . ioTCB . w

getFrontendCfg :: MonadThentosConfig e m => m HttpConfig
getFrontendCfg = do
    Just (feConfig :: HttpConfig) <- (Tagged <$>) <$> getConfigField (Proxy :: Proxy '["frontend"])
    return feConfig

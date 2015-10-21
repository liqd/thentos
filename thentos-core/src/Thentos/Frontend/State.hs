{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Thentos.Frontend.State where

import Control.Monad.Except (throwError, catchError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Control.Monad.State (get, gets, put)
import Data.Char (ord)
import Data.Configifier (Tagged(Tagged), (>>.))
import Data.Functor.Infix ((<$$>))
import Data.Monoid ((<>))
import Data.String.Conversions (SBS, ST, cs)
import LIO (liftLIO)
import LIO.TCB (ioTCB)
import Network.Wai (Middleware, Application)
import Network.Wai.Session (SessionStore, Session, withSession)
import Servant (Proxy(Proxy), ServantErr, (:>), serve, HasServer, ServerT, Server)
import Servant.Server (errHTTPCode, errHeaders, errBody, err303, err404, err400, err500)
import Servant.Server.Internal.Enter ((:~>)(Nat), Enter, enter)
import Servant.Session (SSession)
import System.Log (Priority(DEBUG, ERROR))
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Web.Cookie (SetCookie, def, setCookieName)

import qualified Data.ByteString as SBS
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai.Session.Map as SessionMap

import Thentos.Action.Core
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Frontend.Types
import Thentos.Types (ThentosSessionToken, ThentosError(OtherError))
import Thentos.Util

import qualified Thentos.Frontend.Pages as Pages
import qualified Thentos.Action.Unsafe as U
import qualified Thentos.Action.SimpleAuth as U


-- FIXME: EU-required user notifications about cookies
-- FUTUREWORK: more efficient refresh (only if changed or after 20% of the age has been passed)


-- * errors

crash :: FActionError -> FAction a
crash = undefined -- lift . throwError . OtherError

fActionServantErr :: ActionError FActionError -> IO ServantErr
fActionServantErr = errorInfoToServantErr mkServantErr .
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

    mkServantErr :: ServantErr -> ST -> ServantErr
    mkServantErr baseErr msg = baseErr
        { errBody = cs . renderHtml $ makeErrorPage (errHTTPCode baseErr) msg
        , errHeaders = ("Content-Type", "text/html; charset=utf-8") : errHeaders baseErr
        }

    makeErrorPage :: Int -> ST -> Html
    makeErrorPage 403 = const Pages.permissionDeniedPage
    makeErrorPage 404 = const Pages.notFoundPage
    makeErrorPage _   = Pages.errorPage . cs


-- * middleware

type FSession        = Session IO () FrontendSessionData
type FSessionMap     = Vault.Key FSession -> Maybe FSession
type FSessionStore   = SessionStore IO () FrontendSessionData
type FServantSession = SSession IO () FrontendSessionData

setCookie :: SetCookie
setCookie = def { setCookieName = "thentos" }  -- FIXME: make 'SetCookie' configurable with configifier.

cookieName :: SBS
cookieName = let n = setCookieName setCookie in
    if cookieNameValid n
        then n
        else error $ "Thentos.Frontend.State: bad cookie name: " ++ show n

cookieNameValid :: SBS -> Bool
cookieNameValid = SBS.all (`elem` (fromIntegral . ord <$> '_':['a'..'z']))

thentosSessionMiddleware :: IO (Middleware, Vault.Key FSession)
thentosSessionMiddleware = do
    smap :: FSessionStore <- SessionMap.mapStore_
    key  :: Vault.Key FSession <- Vault.newKey
    return (withSession smap cookieName setCookie key, key)


-- * frontend action monad

serveFAction :: forall api.
        ( HasServer api
        , Enter (ServerT api FAction) (FAction :~> ExceptT ServantErr IO) (Server api)
        )
     => Proxy api -> ServerT api FAction -> ActionState -> IO Application
serveFAction _ fServer aState = thentosSessionMiddleware >>= \(mw, key) -> return (mw $ app key)
  where
    app :: Vault.Key FSession -> Application
    app key = serve (Proxy :: Proxy (FServantSession :> api)) (server' key)

    server' :: Vault.Key FSession -> FSessionMap -> Server api
    server' key smap = enter nt fServer
      where
        nt :: FAction :~> ExceptT ServantErr IO
        nt = enterFAction aState key smap

enterFAction ::
       ActionState
    -> Vault.Key FSession
    -> FSessionMap
    -> FAction :~> ExceptT ServantErr IO
enterFAction aState key smap = Nat $ ExceptT . (>>= fmapLM fActionServantErr) . run
  where
    run :: forall a. FAction a -> IO (Either (ActionError FActionError) a)
    run fServer = fst <$> runActionE emptyFrontendSessionData aState fServer'
      where
        fServer' :: FAction a
        fServer' = do
            case smap key of
                Nothing -> error $ "enterFAction: internal error in servant-session: no cookie!"
                Just (lkup, ins) -> do
                    cookieToFSession (lkup ())
                    updatePrivs
                    fServer `finally`
                        cookieFromFSession (ins ())

    finally :: FAction a -> FAction b -> FAction a
    finally action finalizer = do
        a <- action `catchError` \e -> finalizer >> throwError e
        finalizer >> return a

    updatePrivs :: FAction ()
    updatePrivs = gets l >>= updatePrivs'
      where
        l :: FrontendSessionData -> Maybe ThentosSessionToken
        l (FrontendSessionData (Just (FrontendSessionLoginData t _ _)) _ _) = Just t
        l _ = Nothing

    updatePrivs' :: Maybe ThentosSessionToken -> FAction ()
    updatePrivs' (Just tok) = accessRightsByThentosSession'P tok >>= grantAccessRights'P
    updatePrivs' Nothing    = return ()


-- | Write 'FrontendSessionData' from the servant-session state to 'FAction' state.  If there is no
-- state, do nothing.
cookieToFSession :: IO (Maybe FrontendSessionData) -> FAction ()
cookieToFSession r = liftLIO (ioTCB r) >>= maybe (return ()) put

-- | Read 'FrontendSessionData' from 'FAction' and write back into servant-session state.
cookieFromFSession :: (FrontendSessionData -> IO ()) -> FAction ()
cookieFromFSession w = get >>= liftLIO . ioTCB . w

getFrontendCfg :: FAction HttpConfig
getFrontendCfg = do
    Just (feConfig :: HttpConfig)
        <- (\c -> Tagged <$> c >>. (Proxy :: Proxy '["frontend"]))
            <$> U.unsafeAction U.getConfig
    return feConfig

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
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Control.Monad.State (get, gets, put)
import Data.Char (ord)
import Data.Configifier (Tagged(Tagged), (>>.))
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


type FAction = StateT FrontendSessionData (Action Void)

-- FIXME: where do we put frontend errors?  and where backend errors?  how do we handle them?

runFActionE :: FrontendSessionData -> ActionState -> FAction a
            -> IO (Either (ActionError Void) (a, FrontendSessionData))
runFActionE fState aState fServer = runActionE aState $ runStateT fServer fState


type FSession        = Session IO () FrontendSessionData
type FSessionMap     = Vault.Key FSession -> Maybe FSession
type FSessionStore   = SessionStore IO () FrontendSessionData
type FServantSession = SSession IO () FrontendSessionData


-- * middleware

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
        nt = enterFAction aState baseActionErrorToServantErr key smap


enterFAction ::
       ActionState
    -> (ActionError Void -> IO ServantErr)
    -> Vault.Key FSession
    -> FSessionMap
    -> FAction :~> ExceptT ServantErr IO
enterFAction aState toServantErr key smap = Nat $ ExceptT . (>>= fmapLM toServantErr) . run
  where
    run :: forall a. FAction a -> IO (Either (ActionError Void) a)
    run fServer = fst <$$> runFActionE emptyFrontendSessionData aState fServer'
      where
        fServer' :: FAction a
        fServer' = do
            case smap key of
                Nothing -> error $ "enterFAction: internal error in servant-session: no cookie!"
                Just (lkup, ins) -> do
                    cookieToFSession (lkup ())
                    updatePrivs
                    a <- fServer
                    cookieFromFSession (ins ())
                    return a

    updatePrivs :: FAction ()
    updatePrivs = gets l >>= lift . updatePrivs'
      where
        l :: FrontendSessionData -> Maybe ThentosSessionToken
        l (FrontendSessionData (Just (FrontendSessionLoginData t _)) _ _) = Just t
        l _ = Nothing

    updatePrivs' :: Maybe ThentosSessionToken -> Action e ()
    updatePrivs' (Just tok) = accessRightsByThentosSession'P tok >>= grantAccessRights'P
    updatePrivs' Nothing    = return ()


-- | Write 'FrontendSessionData' from the servant-session state to 'FAction' state.  If there is no
-- state, do nothing.
cookieToFSession :: IO (Maybe FrontendSessionData) -> FAction ()
cookieToFSession r = (lift . liftLIO . ioTCB) r >>= maybe (return ()) put

-- | Read 'FrontendSessionData' from 'FAction' and write back into servant-session state.
cookieFromFSession :: (FrontendSessionData -> IO ()) -> FAction ()
cookieFromFSession w = get >>= lift . liftLIO . ioTCB . w

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Thentos.CookieSession
    ( serveFAction
    , enterFAction
    , noopExtendClearanceOnSessionToken

    -- * types
    , SSession
    , FSession
    , FSessionMap
    , FSessionStore
    , FServantSession
    , FSessionKey
    , ExtendClearanceOnSessionToken
    )
where

import Control.Lens (use)
import Control.Monad (when)
import Control.Monad.Except.Missing (finally)
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Except (ExceptT)
import Crypto.Random (MonadRandom(..))
import Data.Char (ord)
import Data.Maybe (isJust)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions
import Network.Wai (Middleware, Application, vault)
import Network.Wai.Session (SessionStore, Session, withSession)
import Servant (ServantErr, (:>), serve, HasServer, ServerT, Server, (:~>)(Nat), unNat)
import Servant.Server.Internal (route, passToServer)
import Servant.Utils.Enter (Enter, enter)
import Web.Cookie (SetCookie, setCookieName)

import qualified Data.ByteString as SBS
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai.Session.Map as SessionMap

import Servant.Missing (MonadError500, throwError500)
import Thentos.CookieSession.CSRF
import Thentos.CookieSession.Types (ThentosSessionToken, MonadUseThentosSessionToken, getThentosSessionToken)

-- * servant integration

-- | @SSession m k v@ represents a session storage with keys of type @k@,
-- values of type @v@, and operating under the monad @m@.
-- The underlying implementation uses the 'wai-session' package, and any
-- backend compatible with that package should work here too.
data SSession (m :: * -> *) (k :: *) (v :: *)

-- | 'HasServer' instance for 'SSession'.
instance (HasServer sublayout context) => HasServer (SSession n k v :> sublayout) context where
  type ServerT (SSession n k v :> sublayout) m
    = (Vault.Key (Session n k v) -> Maybe (Session n k v)) -> ServerT sublayout m
  route Proxy context subserver =
    route (Proxy :: Proxy sublayout) context (passToServer subserver go)
    where
      go request key = Vault.lookup key $ vault request


-- * middleware

type FSession        fsd = Session IO () fsd
type FSessionMap     fsd = Vault.Key (FSession fsd) -> Maybe (FSession fsd)
type FSessionStore   fsd = SessionStore IO () fsd
type FServantSession fsd = SSession IO () fsd
type FSessionKey     fsd = Vault.Key (FSession fsd)

cookieName :: SetCookie -> SBS
cookieName setCookie =
    if cookieNameValid n
        then n
        else error $ "Thentos.CookieSession: bad cookie name: " ++ show n
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

type ExtendClearanceOnSessionToken m = ThentosSessionToken -> m ()

noopExtendClearanceOnSessionToken :: Monad m => ExtendClearanceOnSessionToken m
noopExtendClearanceOnSessionToken _ = pure ()

-- | The 'ExtendClearanceOnSessionToken' argument can be used in combination with the lio package to
-- write the access policy into the server monad state.  If your way of access restriction has no
-- need for that, simply pass @noopExtendClearanceOnSessionToken@.
serveFAction :: forall api m s e v.
        ( HasServer api '[]
        , Enter (ServerT api m) (m :~> ExceptT ServantErr IO) (Server api)
        , MonadRandom m, MonadError500 e m, MonadHasSessionCsrfToken s m
        , MonadViewCsrfSecret v m, MonadUseThentosSessionToken s m
        )
     => Proxy api
     -> Proxy s
     -> SetCookie
     -> ExtendClearanceOnSessionToken m
     -> IO :~> m
     -> m :~> ExceptT ServantErr IO
     -> ServerT api m -> IO Application
serveFAction _ sProxy setCookie extendClearanceOnSessionToken ioNat nat fServer =
    app <$> sessionMiddleware sProxy setCookie
  where
    app :: (Middleware, FSessionKey s) -> Application
    app (mw, key) = mw $ serve (Proxy :: Proxy (FServantSession s :> api)) (server' key)

    server' :: FSessionKey s -> FSessionMap s -> Server api
    server' key smap = enter nt fServer
      where
        nt :: m :~> ExceptT ServantErr IO
        nt = enterFAction key smap extendClearanceOnSessionToken ioNat nat

enterFAction
    :: ( MonadRandom m, MonadError500 e m, MonadHasSessionCsrfToken s m
       , MonadViewCsrfSecret v m, MonadUseThentosSessionToken s m)
    => FSessionKey s
    -> FSessionMap s
    -> ExtendClearanceOnSessionToken m
    -> IO :~> m
    -> m :~> ExceptT ServantErr IO
    -> m :~> ExceptT ServantErr IO
enterFAction key smap extendClearanceOnSessionToken ioNat nat = Nat $ \fServer -> unNat nat $ do
    case smap key of
        Nothing ->
            -- FIXME: this case should not be code 500, as it can (probably) be provoked by
            -- the client.
            throwError500 "Could not read cookie."
        Just (lkup, ins) -> do
            cookieToFSession ioNat (lkup ())
            maybeSessionToken <- use getThentosSessionToken

            -- update privileges
            mapM_ extendClearanceOnSessionToken maybeSessionToken

            -- refresh the CSRF token if there is a session token
            when (isJust maybeSessionToken) refreshCsrfToken

            fServer `finally` (do
                clearCsrfToken  -- could be replaced by 'refreshCsrfToken'
                cookieFromFSession ioNat (ins ()))

-- | Write 'FrontendSessionData' from the 'SSession' state to 'MonadFAction' state.  If there
-- is no state, do nothing.
cookieToFSession :: MonadState s m => IO :~> m -> IO (Maybe s) -> m ()
cookieToFSession ioNat r = unNat ioNat r >>= mapM_ put

-- | Read 'FrontendSessionData' from 'MonadFAction' and write back into 'SSession' state.
cookieFromFSession :: MonadState s m => IO :~> m -> (s -> IO ()) -> m ()
cookieFromFSession ioNat w = get >>= unNat ioNat . w

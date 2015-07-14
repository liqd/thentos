{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}
{-# LANGUAGE UndecidableInstances                     #-}

module Thentos.Backend.Core
where

import Control.Applicative ((<$>), pure)
import Control.Monad.Trans.Either (EitherT(EitherT))
import Data.CaseInsensitive (CI, mk, foldCase, foldedCase)
import Data.Configifier ((>>.))
import Data.Function (on)
import Data.List (nubBy)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (SBS, ST, cs, (<>))
import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8')
import Data.Typeable (Typeable)
import LIO.Error (AnyLabelError)
import Network.HTTP.Types (Header, methodGet, methodHead)
import Network.Wai (Application, Middleware, Request, requestHeaders, requestMethod)
import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)
import Network.Wai.Internal (Response(..))
import Servant.API ((:>))
import Servant.Server (HasServer, ServerT, ServantErr, route, (:~>)(Nat))
import Servant.Server.Internal (RouteResult(RR))
import Servant.Server.Internal.ServantErr (err400, err401, err403, err404, err500, errBody, responseServantErr)
import System.Log.Logger (Priority(DEBUG, INFO, ERROR, CRITICAL))
import Text.Show.Pretty (ppShow)

import qualified Data.ByteString.Char8 as SBS
import qualified Network.HTTP.Types.Header as HttpTypes

import System.Log.Missing (logger)
import Thentos.Action.Core
import Thentos.Config
import Thentos.Types
import Thentos.Util


-- * action

enterAction :: forall db . (db `Extends` DB, Show (ActionError db),
                            db ~ DB  -- FIXME https://github.com/liqd/thentos/issues/193
                           ) =>
    ActionState db -> Maybe ThentosSessionToken -> Action db :~> EitherT ServantErr IO
enterAction state mTok = Nat $ EitherT . run
  where
    run :: Action db a -> IO (Either ServantErr a)
    run = (>>= fmapLM actionErrorToServantErr) . runActionE state . updatePrivs mTok

    updatePrivs :: Maybe ThentosSessionToken -> Action db a -> Action db a
    updatePrivs (Just tok) action = (accessRightsByThentosSession'P tok >>= grantAccessRights'P) >> action
    updatePrivs Nothing    action = action


-- | Inspect an 'ActionError', log things, and construct a 'ServantErr'.
--
-- If any logging is to take place, it should take place here, not near the place where the error is
-- thrown.  The error constructors should take all the information in typed form.  Rendering
-- (e.g. with 'show') and dispatching different parts of the information to differnet log levels and
-- servant error is the sole responsibility of this function.
actionErrorToServantErr :: forall db . (db `Extends` DB, db ~ DB) => ActionError db -> IO ServantErr
actionErrorToServantErr e = do
    logger DEBUG $ ppShow e
    case e of
        (ActionErrorThentos  te) -> thentosErrorToServantErr te
        (ActionErrorAnyLabel le) -> _permissions le
        (ActionErrorUnknown  _)  -> logger CRITICAL (ppShow e) >> pure err500
  where
    _permissions :: AnyLabelError -> IO ServantErr
    _permissions _ = logger DEBUG (ppShow e) >> pure (err401 { errBody = "unauthorized" })


class ThentosErrorToServantErr db where
    thentosErrorToServantErr :: ThentosError db -> IO ServantErr

instance ThentosErrorToServantErr DB where
  thentosErrorToServantErr e = _thentos e where
    _thentos NoSuchUser = pure $ err404 { errBody = "user not found" }
    _thentos NoSuchPendingUserConfirmation = pure $ err404 { errBody = "unconfirmed user not found" }
    _thentos (MalformedConfirmationToken path) = pure $ err400 { errBody = "malformed confirmation token: " <> cs (show path) }
    _thentos NoSuchService = pure $ err404 { errBody = "service not found" }
    _thentos NoSuchThentosSession = pure $ err404 { errBody = "thentos session not found" }
    _thentos NoSuchServiceSession = pure $ err404 { errBody = "service session not found" }
    _thentos OperationNotPossibleInServiceSession = pure $ err404 { errBody = "operation not possible in service session" }
    _thentos ServiceAlreadyExists = pure $ err403 { errBody = "service already exists" }
    _thentos NotRegisteredWithService = pure $ err403 { errBody = "not registered with service" }
    _thentos UserEmailAlreadyExists = pure $ err403 { errBody = "email already in use" }
    _thentos UserNameAlreadyExists = pure $ err403 { errBody = "user name already in use" }
    _thentos UserIdAlreadyExists = logger ERROR (ppShow e) >> pure err500
    _thentos BadCredentials = logger INFO (show e) >> pure (err401 { errBody = "unauthorized" })
    _thentos BadAuthenticationHeaders = pure $ err400 { errBody = "bad authentication headers" }
    _thentos ProxyNotAvailable = pure $ err404 { errBody = "proxying not activated" }
    _thentos MissingServiceHeader = pure $ err404 { errBody = "headers do not contain service id" }
    _thentos (ProxyNotConfiguredForService sid) = pure $ err404 { errBody = "proxy not configured for service " <> cs (show sid) }
    _thentos (NoSuchToken) = pure $ err404 { errBody = "no such token" }
    _thentos (NeedUserA _ _) = pure $ err404 { errBody = "thentos session belongs to service, cannot create service session" }
    _thentos (MalformedUserPath path) = pure $
        err400 { errBody = "malformed user path: " <> cs (show path) }
    -- the following shouldn't actually reach servant:
    _thentos SsoErrorUnknownCsrfToken = pure $ err500
        { errBody = "invalid token returned during sso process" }
    _thentos (SsoErrorCouldNotAccessUserInfo _) = pure $ err500
        { errBody = "error accessing user info" }
    _thentos (SsoErrorCouldNotGetAccessToken _) = pure $ err500
        { errBody = "error retrieving access token" }


-- * request headers

-- FIXME This is the list of X-Thentos headers used in ALL backends. It would be better to allow
-- each backend to define its own additional headers, but still to preserve type-safety.
data ThentosHeaderName =
    ThentosHeaderSession
  | ThentosHeaderService
  | ThentosHeaderUser
  | ThentosHeaderGroups
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable)

type RenderHeaderFun = ThentosHeaderName -> CI SBS

lookupThentosHeader :: RenderHeaderFun -> Request -> ThentosHeaderName -> Maybe ST
lookupThentosHeader renderHeaderFun req key =
          lookup (renderHeaderFun key) (requestHeaders req)
      >>= either (const Nothing) Just . decodeUtf8'

lookupThentosHeaderSession :: RenderHeaderFun -> Request -> Maybe ThentosSessionToken
lookupThentosHeaderSession renderHeaderFun req =
    ThentosSessionToken <$> lookupThentosHeader renderHeaderFun req ThentosHeaderSession

lookupThentosHeaderService :: RenderHeaderFun -> Request -> Maybe ServiceId
lookupThentosHeaderService renderHeaderFun req =
    ServiceId <$> lookupThentosHeader renderHeaderFun req ThentosHeaderService

-- The default function used to render Thentos-specific header names.
-- Defining alternatives functions allows renaming some or all of the headers.
renderThentosHeaderName :: RenderHeaderFun
renderThentosHeaderName ThentosHeaderSession = mk "X-Thentos-Session"
renderThentosHeaderName ThentosHeaderService = mk "X-Thentos-Service"
renderThentosHeaderName ThentosHeaderUser    = mk "X-Thentos-User"
renderThentosHeaderName ThentosHeaderGroups  = mk "X-Thentos-Groups"

-- | Filter header list for all headers that start with "X-Thentos-", but don't correspond to
-- the default rendering of any 'ThentosHeaderName'.
badHeaders :: [Header] -> [Header]
badHeaders = filter g . filter f
  where
    f (k, _) = foldCase "X-Thentos-" `SBS.isPrefixOf` foldedCase k
    g (k, _) = k `notElem` map renderThentosHeaderName [minBound..]

-- | Remove all headers matched by a 'RenderHeaderFun'.  This is useful if the request is to be
-- used as a basis for e.g. constructing another request to a proxy target.
clearCustomHeaders :: RenderHeaderFun -> HttpTypes.RequestHeaders -> HttpTypes.RequestHeaders
clearCustomHeaders renderHeaderFun = filter $ (`notElem` customHeaderNames) . fst
  where customHeaderNames = map renderHeaderFun [minBound..]

-- | Make sure that all thentos headers are good ('badHeaders' yields empty list).
data ThentosAssertHeaders

instance (HasServer subserver) => HasServer (ThentosAssertHeaders :> subserver)
  where
    type ServerT (ThentosAssertHeaders :> subserver) m = ServerT subserver m

    route Proxy subserver request respond = case badHeaders $ requestHeaders request of
        []  -> route (Proxy :: Proxy subserver) subserver request respond
        bad -> respond . RR . Right . responseServantErr  -- FIXME: use 'left' instead of all this?  yields a type error, though.
             $ err400 { errBody = cs $ "Unknown thentos header fields: " ++ show bad }


-- * response headers

-- | Cache-control headers in HTTP responses.  This is currently just a constant list of headers.
--
-- FUTURE WORK: We may want for this to come from "Thentos.Config".  We may also want to the policy
-- to be a function in the request for which the response is constructed.  Not sure how best to
-- combine these two requirements.
httpCachePolicy :: HttpTypes.ResponseHeaders
httpCachePolicy = [("Cache-Control", "no-cache, no-store, must-revalidate"), ("Expires", "0")]

-- | Add suitable headers to HTTP responses. For now, this just prevents caching of all GET/HEAD
-- requests (other HTTP methods are considered uncacheable by default). In the future we may use
-- more refined caching strategies.
addResponseHeaders :: Middleware
addResponseHeaders app req respond
  | requestMethod req `elem` [methodGet, methodHead] = app req $ respond . updR
  | otherwise                                        = app req respond
  where
    updR (ResponseFile status hdrs filepath part) = ResponseFile status (updH hdrs) filepath part
    updR (ResponseBuilder status hdrs builder)    = ResponseBuilder status (updH hdrs) builder
    updR (ResponseStream status hdrs body)        = ResponseStream status (updH hdrs) body
    updR (ResponseRaw action resp')               = ResponseRaw action (updR resp')
    updH hdrs = nubBy ((==) `on` fst) $ httpCachePolicy ++ hdrs


-- * warp

runWarpWithCfg :: HttpConfig -> Application -> IO ()
runWarpWithCfg cfg = runSettings settings
  where
    settings = setPort (cfg >>. (Proxy :: Proxy '["bind_port"]))
             . setHost hostnameHack
             $ defaultSettings

    -- FIXME: getAddrInfo somewhere behind warp is having difficulties resolving hostnames.  can't
    -- get into this right now.
    hostnameHack = fromString $ case cs $ cfg >>. (Proxy :: Proxy '["bind_host"]) of
        "localhost" -> "127.0.0.1"
        other       -> other

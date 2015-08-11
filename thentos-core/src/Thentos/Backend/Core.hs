{-# LANGUAGE ConstraintKinds                          #-}
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
import Control.Lens ((&), (.~))
import Control.Monad.Trans.Either (EitherT(EitherT))
import Data.Aeson (Value(String), ToJSON(toJSON), (.=), encode, object)
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
import Network.HTTP.Types (Header, methodGet, methodHead, methodPost, ok200, status400)
import Network.Wai (Application, Middleware, Request, requestHeaders, requestMethod)
import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)
import Network.Wai.Internal (Response(..))
import Servant.Docs.Internal (HasDocs(..), sampleByteStrings, response, respTypes, respBody,
        respStatus, single, method, SupportedTypes(..), Method(DocPOST), ToSample(..))
import Servant.API ((:>))
import Servant.API.ContentTypes (AllCTRender, AllMimeRender, IsNonEmpty)
import Servant.Server (HasServer, ServerT, ServantErr, route, (:~>)(Nat))
import Servant.Server.Internal (methodRouter, Router(..), RouteMismatch(HttpError), failWith)
import Servant.Server.Internal.ServantErr (err400, err401, err403, err404, err500, errBody,
        errHeaders)
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

enterAction :: forall db . (db `Ex` DB, ThentosErrorToServantErr db) =>
    ActionState db -> Maybe ThentosSessionToken -> Action db :~> EitherT ServantErr IO
enterAction state mTok = Nat $ EitherT . run
  where
    run :: Action db a -> IO (Either ServantErr a)
    run = (>>= fmapLM actionErrorToServantErr) . runActionE state . updatePrivs mTok

    updatePrivs :: Maybe ThentosSessionToken -> Action db a -> Action db a
    updatePrivs (Just tok) action = (accessRightsByThentosSession'P tok >>= grantAccessRights'P) >> action
    updatePrivs Nothing    action = action


-- * error handling

newtype ErrorMessage = ErrorMessage { fromErrorMessage :: ST }
    deriving (Eq, Show)

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage msg) = object ["status" .= String "error", "error" .= msg]

-- | Construct a ServantErr that looks as our errors should.
-- Status code and reason phrase are taken from the base error given as first argument.
-- The message given as second argument is wrapped into a 'ErrorMessage' JSON object.
mkServantErr :: ServantErr -> ST -> ServantErr
mkServantErr baseErr msg = baseErr { errBody = encode $ ErrorMessage msg
    , errHeaders = [("Content-Type", "application/json; charset=UTF-8")]
    }

-- | Inspect an 'ActionError', log things, and construct a 'ServantErr'.
--
-- If any logging is to take place, it should take place here, not near the place where the error is
-- thrown.  The error constructors should take all the information in typed form.  Rendering
-- (e.g. with 'show') and dispatching different parts of the information to differnet log levels and
-- servant error is the sole responsibility of this function.
actionErrorToServantErr :: forall db . (db `Ex` DB, ThentosErrorToServantErr db)
      => ActionError db -> IO ServantErr
actionErrorToServantErr e = do
    logger DEBUG $ ppShow e
    case e of
        (ActionErrorThentos  te) -> _thentos te
        (ActionErrorAnyLabel le) -> _permissions le
        (ActionErrorUnknown  _)  -> logger CRITICAL (ppShow e) >>
                                    pure (mkServantErr err500 "internal error")
  where
    _thentos :: ThentosError db -> IO ServantErr
    _thentos te = case thentosErrorToServantErr te of
        (Just (level, msg), se) -> logger level msg >> pure se
        (Nothing,           se) ->                     pure se

    _permissions :: AnyLabelError -> IO ServantErr
    _permissions _ = logger DEBUG (ppShow e) >> pure (mkServantErr err401 "unauthorized")


class ThentosErrorToServantErr db where
    thentosErrorToServantErr :: ThentosError db -> (Maybe (Priority, String), ServantErr)

instance ThentosErrorToServantErr DB where
    thentosErrorToServantErr e = f e
      where
        f NoSuchUser =
            (Nothing, mkServantErr err400 "user not found")
        f NoSuchPendingUserConfirmation =
            (Nothing, mkServantErr err400 "unconfirmed user not found")
        f (MalformedConfirmationToken path) =
            (Nothing, mkServantErr err400 $ "malformed confirmation token: " <> cs (show path))
        f NoSuchService =
            (Nothing, mkServantErr err400 "service not found")
        f NoSuchThentosSession =
            (Nothing, mkServantErr err400 "thentos session not found")
        f NoSuchServiceSession =
            (Nothing, mkServantErr err400 "service session not found")
        f OperationNotPossibleInServiceSession =
            (Nothing, mkServantErr err404 "operation not possible in service session")
        f ServiceAlreadyExists =
            (Nothing, mkServantErr err403 "service already exists")
        f NotRegisteredWithService =
            (Nothing, mkServantErr err403 "not registered with service")
        f UserEmailAlreadyExists =
            (Nothing, mkServantErr err403 "email already in use")
        f UserNameAlreadyExists =
            (Nothing, mkServantErr err403 "user name already in use")
        f UserIdAlreadyExists =    -- must be prevented earlier on
            (Just (ERROR, ppShow e), mkServantErr err500 "internal error")
        f BadCredentials =
            (Just (INFO, show e), mkServantErr err401 "unauthorized")
        f BadAuthenticationHeaders =
            (Nothing, mkServantErr err400 "bad authentication headers")
        f ProxyNotAvailable =
            (Nothing, mkServantErr err404 "proxying not activated")
        f MissingServiceHeader =
            (Nothing, mkServantErr err404 "headers do not contain service id")
        f (ProxyNotConfiguredForService sid) =
            (Nothing, mkServantErr err404 $ "proxy not configured for service " <> cs (show sid))
        f (NoSuchToken) =
            (Nothing, mkServantErr err400 "no such token")
        f (NeedUserA _ _) =
            (Nothing, mkServantErr err404
                "thentos session belongs to service, cannot create service session")
        f (MalformedUserPath path) =
            (Nothing, mkServantErr err400 $ "malformed user path: " <> cs (show path))


-- * custom servers for servant

data Post200 (contentTypes :: [*]) a
    deriving Typeable

instance ( AllCTRender ctypes a ) => HasServer (Post200 ctypes a) where
    type ServerT (Post200 ctypes a) m = m a
    route Proxy = methodRouter methodPost (Proxy :: Proxy ctypes) ok200

instance (ToSample a b, IsNonEmpty cts, AllMimeRender cts b, SupportedTypes cts)
    => HasDocs (Post200 cts a) where
  docsFor Proxy (endpoint, action) = single endpoint' action'

    where endpoint' = endpoint & method .~ DocPOST
          action' = action & response.respBody .~ sampleByteStrings t p
                           & response.respTypes .~ supportedTypes t
                           & response.respStatus .~ 200
          t = Proxy :: Proxy cts
          p = Proxy :: Proxy a

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

    route Proxy subserver = WithRequest $ \ request -> route (Proxy :: Proxy subserver) $
       case badHeaders $ requestHeaders request of
          []  -> subserver
          bad -> return $ failWith $ HttpError status400 (Just . cs $ "Unknown thentos header fields: " ++ show bad)


-- * response headers

-- | Cache-control headers in HTTP responses.  This is currently just a constant list of headers.
--
-- FUTURE WORK: We may want for this to come from "Thentos.Config".  We may also want to the policy
-- to be a function in the request for which the response is constructed.  Not sure how best to
-- combine these two requirements.
httpCachePolicy :: HttpTypes.ResponseHeaders
httpCachePolicy = [("Cache-Control", "no-cache, no-store, must-revalidate"), ("Expires", "0")]

-- | Add suitable cache-control headers to HTTP responses. For now, this just prevents caching of
-- all GET/HEAD requests (other HTTP methods are considered uncacheable by default). In the future
-- we may use more refined caching strategies.
addCacheControlHeaders :: Middleware
addCacheControlHeaders app req respond
  | requestMethod req `elem` [methodGet, methodHead] = app req $
        respond . addHeadersToResponse httpCachePolicy
  | otherwise                                        = app req respond

addHeadersToResponse ::  HttpTypes.ResponseHeaders -> Response -> Response
addHeadersToResponse extraHeaders resp = case resp of
    ResponseFile status hdrs filepath part -> ResponseFile status (updH hdrs) filepath part
    ResponseBuilder status hdrs builder    -> ResponseBuilder status (updH hdrs) builder
    ResponseStream status hdrs body        -> ResponseStream status (updH hdrs) body
    ResponseRaw action resp'               -> ResponseRaw action $
                                                  addHeadersToResponse extraHeaders resp'
  where
    updH hdrs = nubBy ((==) `on` fst) $ extraHeaders ++ hdrs

-- | Policy for Cross-origin Resource Sharing (CORS).
data CorsPolicy = CorsPolicy { corsHeaders :: SBS, corsMethods :: SBS, corsOrigin :: SBS }

-- | Add "Access-Control-Allow-..." headers based on a 'CorsPolicy'.
addCorsHeaders :: CorsPolicy -> Middleware
addCorsHeaders policy app req respond = app req $
        respond . addHeadersToResponse accessControlHeaders
  where
    accessControlHeaders =
       [ ("Access-Control-Allow-Headers", corsHeaders policy)
       , ("Access-Control-Allow-Methods", corsMethods policy)
       , ("Access-Control-Allow-Origin", corsOrigin policy)
       ]

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

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Thentos.Backend.Core
where

import Thentos.Prelude hiding ((.=))
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Data.Aeson (Value(String), ToJSON(toJSON), (.=), encode, object)
import Data.CaseInsensitive (CI, mk, foldCase, foldedCase)
import Data.Configifier ((>>.))
import Data.Text.Encoding (decodeUtf8')
import Network.HTTP.Types (Header, methodGet, methodHead, statusCode)
import Network.Wai (Application, Middleware, Request, requestHeaders, requestMethod,
                    responseHeaders, responseStatus)
import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)
import Network.Wai.Internal (Response(ResponseFile, ResponseBuilder, ResponseStream, ResponseRaw))
import Servant.API ((:>))
import Servant.Server (HasServer, ServerT, ServantErr, route, (:~>)(Nat))
import Servant.Server.Internal.RoutingApplication (addMethodCheck, withRequest, delayedFailFatal)
import Servant.Server.Internal.ServantErr
    ( err400, err401, err403, err404, err500, errBody
    , errHeaders
    )
import Servant.Utils.Links (HasLink(MkLink, toLink))

import qualified Data.ByteString.Char8 as SBS
import qualified Blaze.ByteString.Builder as Builder
import qualified Data.Text as ST
import qualified Network.HTTP.Types.Header as HttpTypes
import qualified Servant.Foreign as F

import Thentos.Action.Core
import Thentos.Action.TCB
import Thentos.Action.Types
import Thentos.Backend.Api.Auth.Types
import Thentos.Config
import Thentos.Types
import Thentos.Util

-- * action

enterAction :: forall s e. (Show e, Typeable e) =>
    s -> ActionEnv ->
    (ActionError e -> IO ServantErr) ->
    ThentosAuthCredentials -> ActionStack e s :~> ExceptT ServantErr IO
enterAction polyState actionEnv toServantErr creds = Nat $ ExceptT . run toServantErr
  where
    run :: (Show e, Typeable e)
        => (ActionError e -> IO ServantErr)
        -> ActionStack e s a -> IO (Either ServantErr a)
    run e = (>>= _Left e . fst) . runActionE polyState actionEnv . (extendClearanceOnThentosAuthCredentials creds >>)


-- * error handling

newtype ErrorMessage = ErrorMessage { fromErrorMessage :: ST }
    deriving (Eq, Show)

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage msg) = object ["status" .= String "error", "error" .= msg]

-- | Construct a ServantErr that looks as our errors should.
-- Status code and reason phrase are taken from the base error given as first argument.
-- The message given as second argument is wrapped into a 'ErrorMessage' JSON object.
mkServantErr :: ServantErr -> ST -> ServantErr
mkServantErr baseErr msg = baseErr
    { errBody = encode $ ErrorMessage msg
    , errHeaders = nubBy ((==) `on` fst) $ contentTypeJsonHeader : errHeaders baseErr
    }

type ErrorInfo a = (Maybe (Priority, String), ServantErr, a)

-- | Convert plain-text errors not caught by 'mkServantErr' into proper JSON objects.
-- This concerns e.g. errors thrown by aeson if the request body cannot be decoded as JSON into
-- the expected type. If the response status indicates that an error occurred (400 or higher)
-- and no Content-Type is set, we set it to "application/json" and wrap the response body (i.e.,
-- error message) in an 'ErrorMessage' object.
jsonifyPlainTextErrors :: Middleware
jsonifyPlainTextErrors app req respond = app req $ respond . jsonifyErrorResponse
  where
    jsonifyErrorResponse :: Response -> Response
    jsonifyErrorResponse resp | isErrorWithoutContentType resp = addHeaderAndConvertBody resp
                              | otherwise                      = resp
    isErrorWithoutContentType :: Response -> Bool
    isErrorWithoutContentType resp = statusCode (responseStatus resp) >= 400 &&
                                     "Content-Type" `notElem` map fst (responseHeaders resp)
    addHeaderAndConvertBody :: Response -> Response
    addHeaderAndConvertBody resp = case resp of
        ResponseBuilder status hdrs builder ->
            ResponseBuilder status (updH hdrs) (updBuilder builder)
        -- FIXME Do we need to handle ResponseStream and ResponseFile too? ResponseBuilder seems
        -- to be the typical case
        r@(ResponseStream _ _ _) -> r
        r@(ResponseFile _ _ _ _) -> r
        ResponseRaw action resp' -> ResponseRaw action $ addHeaderAndConvertBody resp'
    updH hdrs  = contentTypeJsonHeader : hdrs
    updBuilder = Builder.fromByteString . cs . encode . ErrorMessage . cs . Builder.toByteString

-- | Log things, and construct a 'ServantErr'.
--
-- If any logging is to take place, it should take place here, not near the place where the error is
-- thrown.
errorInfoToServantErr :: (ServantErr -> a -> ServantErr) -> ErrorInfo a -> IO ServantErr
errorInfoToServantErr mkServErr (l, se, x) = for_ l (uncurry logger) $> mkServErr se x

baseActionErrorToServantErr :: ActionError Void -> IO ServantErr
baseActionErrorToServantErr = errorInfoToServantErr mkServantErr .
                                 actionErrorInfo (thentosErrorInfo absurd)

actionErrorInfo :: Show e => (ThentosError e -> ErrorInfo ST) -> ActionError e -> ErrorInfo ST
actionErrorInfo thentosInfo e =
    case e of
        ActionErrorThentos  te -> thentosInfo te
        ActionErrorAnyLabel _  -> (Just (DEBUG, ppShow e), err401, "unauthorized")
        ActionErrorUnknown  _  -> (Just (CRITICAL, ppShow e), err500, "internal error")

thentosErrorInfo :: Show e
                 => (e -> ErrorInfo ST)
                 -> ThentosError e
                 -> ErrorInfo ST
thentosErrorInfo other e = f e
  where
    f NoSuchUser =
        (Nothing, err404, "user not found")
    f NoSuchPendingUserConfirmation =
        (Nothing, err400, "unconfirmed user not found")
    f (MalformedConfirmationToken path) =
        (Nothing, err400, "malformed confirmation token: " <> cs (show path))
    f NoSuchService =
        (Nothing, err400, "service not found")
    f NoSuchThentosSession =
        (Nothing, err400, "thentos session not found")
    f NoSuchServiceSession =
        (Nothing, err400, "service session not found")
    f NoSuchPersona =
        (Nothing, err400, "persona not found")
    f NoSuchContext =
        (Nothing, err400, "context not found")
    f MultiplePersonasPerContext =
        (Nothing, err400, "Sybil attack prevention: "
            <> "cannot register multiple personas of the same user for the same context")
    f (GroupMembershipLoop subgroup supergroup) =
        (Nothing, err400, ST.concat ["Adding ", cshow subgroup, " to ", cshow supergroup,
                                     " would result in a group membership loop"])
    f OperationNotPossibleInServiceSession =
        (Nothing, err404, "operation not possible in service session")
    f ServiceAlreadyExists =
        (Nothing, err403, "service already exists")
    f NotRegisteredWithService =
        (Nothing, err403, "not registered with service")
    f UserEmailAlreadyExists =
        (Nothing, err403, "email already in use")
    f UserNameAlreadyExists =
        (Nothing, err403, "user name already in use")
    f PersonaNameAlreadyExists =
        (Nothing, err403, "persona name already in use")
    f ContextNameAlreadyExists =
        (Nothing, err403, "context name already in use")
    f CaptchaIdAlreadyExists =
        (Nothing, err500, "duplicate captcha ID")
    f NoSuchCaptchaId =
        (Nothing, err400, "unknown captcha ID")
    f (AudioCaptchaVoiceNotFound voice) =
        (Nothing, err404, "unknown audio captcha voice name: " <> cs voice)
    f (AudioCaptchaInternal _ _ _) =
        (Just (ERROR, ppShow e), err500, "unknown error during audio captcha construction")
    f BadCredentials =
        (Just (INFO, show e), err401, "unauthorized")
    f BadAuthenticationHeaders =
        (Nothing, err400, "bad authentication headers")
    f ProxyNotAvailable =
        (Nothing, err404, "proxying not activated")
    f MissingServiceHeader =
        (Nothing, err404, "headers do not contain service id")
    f (ProxyNotConfiguredForService sid) =
        (Nothing, err404, "proxy not configured for service " <> cs (show sid))
    f NoSuchToken =
        (Nothing, err400, "no such token")
    f (NeedUserA _ _) =
        (Nothing, err404,
            "thentos session belongs to service, cannot create service session")
    f InvalidCaptchaSolution =
        (Nothing, err400, "invalid solution supplied for captcha")
    f (MalformedUserPath path) =
        (Nothing, err400, "malformed user path: " <> cs (show path))
    f ConfirmationTokenAlreadyExists =
        (Just (ERROR, ppShow e), err500, "internal error")
    f PasswordTooShort =
        (Nothing, err400,
            "password too short (less than " <> cs (show minPasswordLength) <> " characters)")
    f (OtherError x) = other x


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

-- | The default function used to render Thentos-specific header names.
--
-- FIXME: this needs to move to the "Thentos.Backend.Api.Auth", and become a type family so it can
-- actually be overridden for other apis, like thentos-adhocracy.  (See also: comment on
-- thentos-adhocracy's 'emptyCreds'.)
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

instance HasServer subserver context => HasServer (ThentosAssertHeaders :> subserver) context
  where
    type ServerT (ThentosAssertHeaders :> subserver) m = ServerT subserver m

    route Proxy context subserver =
        route (Proxy :: Proxy subserver) context $ addMethodCheck subserver go
      where
        go = withRequest $ \request -> case badHeaders $ requestHeaders request of
                []  -> return ()
                bad -> delayedFailFatal err400 { errBody = cs $ "Unknown thentos header fields: " ++ show bad}

instance HasLink sub => HasLink (ThentosAssertHeaders :> sub) where
    type MkLink (ThentosAssertHeaders :> sub) = MkLink sub
    toLink _ = toLink (Proxy :: Proxy sub)

instance F.HasForeign flang ftype sub
      => F.HasForeign flang ftype (ThentosAssertHeaders :> sub) where
    type Foreign ftype (ThentosAssertHeaders :> sub) = F.Foreign ftype sub
    foreignFor plang Proxy Proxy = F.foreignFor plang Proxy (Proxy :: Proxy sub)


-- * response headers

-- | header setting the Content-Type to JSON.
contentTypeJsonHeader ::  Header
contentTypeJsonHeader = ("Content-Type", "application/json")

-- | Cache-control headers in HTTP responses.  This is currently just a constant list of headers.
--
-- FUTUREWORK: We may want for this to come from "Thentos.Config".  We may also want to the policy
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

-- * warp & wai

-- FIXME: move this to a module for use both by frontend and backend.
runWarpWithCfg :: HttpConfig -> Application -> IO ()
runWarpWithCfg cfg = runSettings settings . jsonifyPlainTextErrors
  where
    settings = setPort (cfg >>. (Proxy :: Proxy '["bind_port"]))
             . setHost hostnameHack
             $ defaultSettings

    -- FIXME: getAddrInfo somewhere behind warp is having difficulties resolving hostnames.  can't
    -- get into this right now.
    hostnameHack = fromString $ case cs $ cfg >>. (Proxy :: Proxy '["bind_host"]) of
        "localhost" -> "127.0.0.1"
        other       -> other

-- | Write all requests and responses to log file with prio 'DEBUG'.  Since 'Response' does not have
-- a 'Show' instance, only dump the showable parts.
loggerMW :: Middleware
loggerMW app req cont = do
    logger DEBUG $ "serviceProxy response: " ++ show req
    app req (\resp -> do
        logger DEBUG $ "serviceProxy response: " ++ show_ resp
        cont resp)
  where
    show_ (ResponseFile s hs fp mfpart) = "ResponseFile " ++ show (s, hs, fp, mfpart)
    show_ (ResponseBuilder s hs _) = "ResponseBuilder " ++ show (s, hs)
    show_ (ResponseStream s hs _) = "ResponseStream " ++ show (s, hs)
    show_ (ResponseRaw _ resp) = "ResponseRaw " ++ show_ resp

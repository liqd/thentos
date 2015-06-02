{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE PackageImports                           #-}
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
import Data.Char (isUpper)
import Data.Configifier ((>>.))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs, (<>))
import Data.String.Conversions (SBS, ST)
import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8')
import Data.Typeable (Typeable)
import LIO.Error (AnyLabelError)
import Network.HTTP.Types (Header)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)
import Network.Wai (Request, requestHeaders)
import Servant.API ((:>), Get)
import Servant.Server (HasServer, ServerT, ServantErr, route, (:~>)(Nat))
import Servant.Server.Internal (RouteResult(RR))
import Servant.Server.Internal.ServantErr (err400, err401, err403, err404, err500, errBody, responseServantErr)
import System.Log.Logger (Priority(DEBUG, INFO, CRITICAL))
import Text.Show.Pretty (ppShow)

import qualified Data.ByteString.Char8 as SBS
import qualified Network.HTTP.Types.Header as HttpTypes

import System.Log.Missing (logger)
import Thentos.Action.Core
import Thentos.Config
import Thentos.Types
import Thentos.Util


-- * action

enterAction :: ActionState DB -> Maybe ThentosSessionToken -> Action DB :~> EitherT ServantErr IO
enterAction state mTok = Nat $ EitherT . run
  where
    run :: Action DB a -> IO (Either ServantErr a)
    run = (>>= fmapLM actionErrorToServantErr) . runActionE state . updatePrivs mTok

    updatePrivs :: Maybe ThentosSessionToken -> Action DB a -> Action DB a
    updatePrivs (Just tok) action = (accessRightsByThentosSession'P tok >>= grantAccessRights'P) >> action
    updatePrivs Nothing    action = action


-- | Inspect an 'ActionError', log things, and construct a 'ServantErr'.
actionErrorToServantErr :: ActionError -> IO ServantErr
actionErrorToServantErr e = do
    logger DEBUG $ ppShow e
    case e of
        (ActionErrorThentos  te) -> _thentos te
        (ActionErrorAnyLabel le) -> _permissions le
        (ActionErrorUnknown  _)  -> logger CRITICAL (ppShow e) >> pure err500
  where
    _thentos :: ThentosError -> IO ServantErr
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
    _thentos BadCredentials = logger INFO (show e) >> pure (err401 { errBody = "unauthorized" })
    _thentos BadAuthenticationHeaders = pure $ err400 { errBody = "bad authentication headers" }
    _thentos ProxyNotAvailable = pure $ err404 { errBody = "proxying not activated" }
    _thentos MissingServiceHeader = pure $ err404 { errBody = "headers do not contain service id" }
    _thentos (ProxyNotConfiguredForService sid) = pure $ err404 { errBody = "proxy not configured for service " <> cs (show sid) }
    _thentos (NoSuchToken) = pure $ err404 { errBody = "no such token" }
    _thentos (NeedUserA _ _) = pure $ err404 { errBody = "thentos session belongs to service, cannot create service session" }

    _permissions :: AnyLabelError -> IO ServantErr
    _permissions _ = logger DEBUG (ppShow e) >> pure (err401 { errBody = "unauthorized" })


-- * request header

data ThentosHeaderName =
    ThentosHeaderSession
  | ThentosHeaderService
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable)

lookupThentosHeader :: Request -> ThentosHeaderName -> Maybe ST
lookupThentosHeader req key =
          lookup (renderThentosHeaderName key) (requestHeaders req)
      >>= either (const Nothing) Just . decodeUtf8'

lookupThentosHeaderSession :: Request -> Maybe ThentosSessionToken
lookupThentosHeaderSession req = ThentosSessionToken <$> lookupThentosHeader req ThentosHeaderSession

lookupThentosHeaderService :: Request -> Maybe ServiceId
lookupThentosHeaderService req = ServiceId <$> lookupThentosHeader req ThentosHeaderService

renderThentosHeaderName :: ThentosHeaderName -> CI SBS
renderThentosHeaderName x = case splitAt (SBS.length "ThentosHeader") (show x) of
    ("ThentosHeader", s) -> mk . SBS.pack $ "X-Thentos" ++ dashify s
    bad -> error $ "renderThentosHeaderName: prefix (left side) must be \"ThentosHeader\" in " ++ show bad
  where
    dashify ""    = ""
    dashify (h:t) = if isUpper h
        then '-' : h : dashify t
        else       h : dashify t

-- | Filter header list for all headers that start with "X-Thentos-", but have no parse in
-- 'ThentosHeaderName'.
badHeaders :: [Header] -> [Header]
badHeaders = filter g . filter f
  where
    f (k, _) = foldCase "X-Thentos-" `SBS.isPrefixOf` foldedCase k
    g (k, _) = not $ k `elem` map renderThentosHeaderName [minBound..]

-- | Remove all headers that match @X-Thentos-.*@.  This is useful if the request is to be used as a
-- basis for e.g. constructing another request to a proxy target.
clearThentosHeaders :: HttpTypes.RequestHeaders -> HttpTypes.RequestHeaders
clearThentosHeaders = filter $ (foldedCase "X-Thentos-" `SBS.isPrefixOf`) . foldedCase . fst

-- | Make sure that all thentos headers are good ('badHeaders' yields empty list).
data ThentosAssertHeaders = ThentosAssertHeaders

instance (HasServer subserver) => HasServer (ThentosAssertHeaders :> subserver)
  where
    type ServerT (ThentosAssertHeaders :> subserver) m = ServerT subserver m

    route Proxy subserver request respond = case badHeaders $ requestHeaders request of
        []  -> route (Proxy :: Proxy subserver) subserver request respond
        bad -> respond . RR . Right . responseServantErr  -- FIXME: use 'left' instead of all this?  yields a type error, though.
             $ err400 { errBody = cs $ "Unknown thentos header fields: " ++ show bad }


-- * response header

type CGet ctyps val = Servant.API.Get ctyps val

instance (HasServer (Get ctyps val)) => HasServer (CGet ctyps val)
  where
    type ServerT (CGet ctyps val) m = ServerT (Get ctyps val) m

    route Proxy subserver request respond =
        route (Proxy :: Proxy subserver) subserver request $ \ response ->
            respond (setHeaders [("Cache", "god no!"), ("Virus-Scanned", False)] response)


-- * warp

runWarpWithCfg :: HttpConfig -> Application -> IO ()
runWarpWithCfg cfg = runSettings settings
  where
    settings = setPort (cfg >>. (Proxy :: Proxy '["bind_port"]))
             . setHost (fromString . cs $ cfg >>. (Proxy :: Proxy '["bind_host"]))
             $ defaultSettings

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
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}
{-# LANGUAGE UndecidableInstances                     #-}

module Thentos.Backend.Api.Proxy (ServiceProxy, serviceProxy) where

import Control.Applicative ((<$>))
import Control.Exception (Exception)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (lift)
import Control.Monad.Trans.Either (EitherT(EitherT), runEitherT, left)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT, ask)
import Data.CaseInsensitive (foldedCase)
import Data.Configifier (Tagged(Tagged), (>>.))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (deriveSafeCopy, base)
import Data.String.Conversions (ST, LBS, cs)
import Data.Typeable (Typeable)
import Servant.API (Raw)
import Servant.Server.Internal (Server)
import System.Log.Logger (Priority(DEBUG))

import qualified Data.ByteString as SBS
import qualified Data.Map as Map
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types.Status as T
import qualified Network.HTTP.Types.Header as T
import qualified Network.Wai as S

import Thentos.Api
import Thentos.Backend.Core
import Thentos.DB
import Thentos.Types
import Thentos.Config
import System.Log.Missing (logger)


-- * types

type ServiceProxy = Raw


data ProxyNotAvailable = ProxyNotAvailable
  deriving (Eq, Ord, Show, Read, Typeable)

data ProxyNotConfiguredForService = ProxyNotConfiguredForService ServiceId
  deriving (Eq, Ord, Show, Read, Typeable)

data MissingServiceHeader = MissingServiceHeader
  deriving (Eq, Ord, Show, Read, Typeable)


instance ThentosError ProxyNotAvailable
instance Exception ProxyNotAvailable
$(deriveSafeCopy 0 'base ''ProxyNotAvailable)
instance ThentosErrorServant ProxyNotAvailable where
    renderErrorServant ProxyNotAvailable = (404, "proxying not activated")

instance ThentosError ProxyNotConfiguredForService
instance Exception ProxyNotConfiguredForService
$(deriveSafeCopy 0 'base ''ProxyNotConfiguredForService)
instance ThentosErrorServant ProxyNotConfiguredForService where
    renderErrorServant (ProxyNotConfiguredForService sid) = (404, "proxy not configured for service " ++ show sid)

instance ThentosError MissingServiceHeader
instance Exception MissingServiceHeader
$(deriveSafeCopy 0 'base ''MissingServiceHeader)
instance ThentosErrorServant MissingServiceHeader where
    renderErrorServant MissingServiceHeader = (404, "headers do not contain service id")


-- * handlers

serviceProxy :: PushActionSubRoute (Server ServiceProxy)
serviceProxy req cont = catchProxy cont $ do
    rqMod <- getRqMod req
    liftIO $ C.withManager C.defaultManagerSettings $ \ manager ->
        prepareReq rqMod req >>=
        (`C.httpLbs` manager) >>=
        cont . prepareResp

prepareReq :: RqMod -> S.Request -> IO C.Request
prepareReq (RqMod target proxyHdrs) req = do
    body <- S.strictRequestBody req
    req' <- C.parseUrl $ target ++ (cs $ S.rawPathInfo req)
    return . C.setQueryString (S.queryString req) $ req'
        { C.method         = S.requestMethod req
        , C.checkStatus    = \ _ _ _ -> Nothing
        , C.requestBody    = C.RequestBodyLBS body
        , C.requestHeaders = proxyHdrs <> clearThentosHeaders (S.requestHeaders req)
        }

prepareResp :: C.Response LBS -> S.Response
prepareResp res = S.responseLBS (C.responseStatus res) (C.responseHeaders res) (C.responseBody res)

-- | Remove all headers that match @X-Thentos-.*@.
clearThentosHeaders :: T.RequestHeaders -> T.RequestHeaders
clearThentosHeaders = filter $ (foldedCase "X-Thentos-" `SBS.isPrefixOf`) . foldedCase . fst

-- | Request modifier that contains all information that is needed to
-- alter and forward an incoming request.
data RqMod = RqMod String T.RequestHeaders
  deriving (Eq, Show)

-- | Extract proxy config from thentos config.  Look up session from
-- the token provided in the request header @X-Thentos-Session@ and
-- fill headers @X-Thentos-User@, @X-Thentos-Groups@.  If
-- 'proxyConfig' is 'Nothing' or an invalid or inactive session token
-- is provided, throw an error.
getRqMod :: S.Request -> Action r RqMod
getRqMod req = do
    ((_, _, thentosConfig), _) <- ask

    prxCfg :: Map.Map ServiceId HttpProxyConfig
        <- case getProxyConfigMap thentosConfig of
            Nothing -> lift . left . toThentosError $ ProxyNotAvailable
            Just v  -> return v

    hdrs <- do
        (_, session) <- maybe (lift . left . toThentosError $ NoSuchSession) (bumpSession . SessionToken) $
            lookupRequestHeader req ThentosHeaderSession
        (_, user) <- case session ^. sessionAgent of
            UserA uid  -> queryAction $ LookupUser uid
            ServiceA _ -> lift . left . toThentosError $ NoSuchUser

        let newHdrs =
                ("X-Thentos-User", cs . fromUserName $ user ^. userName) :
                ("X-Thentos-Groups", cs . show $ user ^. userGroups) :
                []
        return newHdrs

    sid <- case lookupRequestHeader req ThentosHeaderService of
            Just s  -> return $ ServiceId s
            Nothing -> lift . left . toThentosError $ MissingServiceHeader

    target :: String
        <- case Map.lookup sid prxCfg of
            Just t  -> let http :: HttpConfig = Tagged $ t >>. (Proxy :: Proxy '["http"])
                           prefix :: ST = fromMaybe "" $ t >>. (Proxy :: Proxy '["url_prefix"])
                       in return . cs $ exposeUrl http <> prefix
            Nothing -> lift . left . toThentosError $ ProxyNotConfiguredForService sid

    let rqMod = RqMod target hdrs
    logger DEBUG $ "forwarding proxy request with modifier: " ++ show rqMod
    return rqMod


-- | This is a work-around for the fact that we can't write an
-- instance for 'Application'.  See FIXME in the corresponding
-- 'PushActionC' instance.
--
-- (There is some redundancy between here and 'catchAction'.  Not sure
-- if we should spend time cleaning that up, or if we should wait for
-- something to happen to 'Raw' that cleans up this function
-- automatically.)
catchProxy :: forall r . (r ~ S.ResponseReceived) => (S.Response -> IO r) -> RestAction r -> RestAction r
catchProxy cont action =
    ReaderT $ \ state ->
        EitherT $ do
            outcome <- runEitherT $ action `runReaderT` state
            case outcome of
                Left e -> renderError e >>= \ (status, msg) -> Right <$>
                              cont (S.responseLBS (T.Status status (cs msg)) [] (cs msg))
                Right r -> return $ Right r

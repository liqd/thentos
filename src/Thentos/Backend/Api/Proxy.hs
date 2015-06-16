{-# LANGUAGE DataKinds                                #-}
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

module Thentos.Backend.Api.Proxy where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Data.Configifier (Tagged(Tagged), (>>.))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, LBS, cs)
import Servant.API (Raw)
import Servant.Server (Server, HasServer(..))
import Servant.Server.Internal.ServantErr (responseServantErr)
import System.Log.Logger (Priority(DEBUG))

import qualified Data.Map as Map
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types.Header as T
import qualified Network.Wai as S

import Thentos.Action
import Thentos.Action.Core
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Types

data ServiceProxy

instance HasServer ServiceProxy where
  type ServerT ServiceProxy m = S.Application
  route Proxy l = route (Proxy :: Proxy Raw) l

serviceProxy :: ActionState DB -> Server ServiceProxy
serviceProxy state req cont = do
    eRqMod <- runActionE state $ getRqMod req
    case eRqMod of
        Right rqMod -> do
            C.withManager C.defaultManagerSettings $ \ manager ->
                prepareReq rqMod req >>=
                (`C.httpLbs` manager) >>=
                cont . prepareResp
        Left e -> do
            servantError <- actionErrorToServantErr e
            cont $ responseServantErr servantError

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


-- | Request modifier that contains all information that is needed to
-- alter and forward an incoming request.
data RqMod = RqMod String T.RequestHeaders
  deriving (Eq, Show)

-- | Extract proxy config from thentos config.  Look up session from
-- the token provided in the request header @X-Thentos-Session@ and
-- fill headers @X-Thentos-User@, @X-Thentos-Groups@.  If
-- 'proxyConfig' is 'Nothing' or an invalid or inactive session token
-- is provided, throw an error.
getRqMod :: S.Request -> Action DB RqMod
getRqMod req = do
    thentosConfig <- getConfig'P

    prxCfg :: Map.Map ServiceId HttpProxyConfig
        <- maybe (throwError ProxyNotAvailable) return $ getProxyConfigMap thentosConfig
    (uid, user) :: (UserId, User)
        <- do
            tok <- maybe (throwError NoSuchThentosSession) return
                         (lookupThentosHeaderSession req)
            session <- lookupThentosSession tok
            case session ^. thSessAgent of
                UserA uid  -> lookupUser uid
                ServiceA sid -> throwError $ NeedUserA tok sid

    sid :: ServiceId
        <- case lookupThentosHeaderService req of
               Just s  -> return s
               Nothing -> throwError MissingServiceHeader

    groups :: [Group]
        <- userGroups uid sid

    let hdrs =
            ("X-Thentos-User", cs . fromUserName $ user ^. userName) :
            ("X-Thentos-Groups", cs $ show groups) :
            []

    target :: String
        <- case Map.lookup sid prxCfg of
            Just t  -> let http :: HttpConfig = Tagged $ t >>. (Proxy :: Proxy '["http"])
                           prefix :: ST = fromMaybe "" $ t >>. (Proxy :: Proxy '["url_prefix"])
                       in return . cs $ exposeUrl http <> prefix
            Nothing -> throwError $ ProxyNotConfiguredForService sid

    let rqMod = RqMod target hdrs
    logger'P DEBUG $ "forwarding proxy request with modifier: " ++ show rqMod
    return rqMod

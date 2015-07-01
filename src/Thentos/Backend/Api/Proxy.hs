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

import Control.Applicative ((<$>))
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
  route Proxy = route (Proxy :: Proxy Raw)

serviceProxy :: RenderHeaderFun -> ActionState DB -> Server ServiceProxy
serviceProxy renderHeaderFun state req cont = do
    eRqMod <- runActionE state $ getRqMod renderHeaderFun req
    case eRqMod of
        Right rqMod -> do
            C.withManager C.defaultManagerSettings $ \ manager ->
                prepareReq renderHeaderFun rqMod req >>=
                (`C.httpLbs` manager) >>=
                cont . prepareResp
        Left e -> do
            servantError <- actionErrorToServantErr e
            cont $ responseServantErr servantError

prepareReq :: RenderHeaderFun -> RqMod -> S.Request -> IO C.Request
prepareReq renderHeaderFun (RqMod target proxyHdrs) req = do
    body <- S.strictRequestBody req
    req' <- C.parseUrl $ target ++ (cs $ S.rawPathInfo req)
    return . C.setQueryString (S.queryString req) $ req'
        { C.method         = S.requestMethod req
        , C.checkStatus    = \ _ _ _ -> Nothing
        , C.requestBody    = C.RequestBodyLBS body
        , C.requestHeaders = proxyHdrs <> clearCustomHeaders renderHeaderFun (S.requestHeaders req)
        }

prepareResp :: C.Response LBS -> S.Response
prepareResp res = S.responseLBS (C.responseStatus res) (C.responseHeaders res) (C.responseBody res)


-- | Request modifier that contains all information that is needed to
-- alter and forward an incoming request.
data RqMod = RqMod String T.RequestHeaders
  deriving (Eq, Show)

-- | Create request modifier with custom headers to add to it and the target URL of the
-- proxied app to forward it to.
--
-- If the request contains a @X-Thentos-Service@ header, we find the proxied app based on
-- this header -- an error is thrown if the "proxies" section of the config doesn't match.
-- Otherwise, the default proxied app from the "proxy" section of the config is used --
-- an error is thrown if that section is missing.
--
-- If the request contains a @X-Thentos-Session@ header, we validate the session and set the
-- @X-Thentos-User@ and @X-Thentos-Groups@ headers accordingly. Otherwise the request is
-- forwarded as an anonymous request (no user logged in).
--
-- The first parameter is a function that can be used to rename the Thentos-specific headers.
-- To stick with the default names, use 'Thentos.Backend.Core.renderThentosHeaderName'.
getRqMod :: RenderHeaderFun -> S.Request -> Action DB RqMod
getRqMod renderHeaderFun req = do
    thentosConfig <- getConfig'P
    let mTok = lookupThentosHeaderSession renderHeaderFun req

    (sid, target) <- case lookupThentosHeaderService renderHeaderFun req of
        Just s  -> findTargetForServiceId s thentosConfig
        Nothing -> findDefaultServiceIdAndTarget thentosConfig

    hdrs <- createCustomHeaders renderHeaderFun mTok sid
    let rqMod = RqMod target hdrs
    logger'P DEBUG $ "forwarding proxy request with modifier: " ++ show rqMod
    return rqMod

-- | Look up the target URL for requests based on the given service ID. This requires a "proxies"
-- section in the config. An error is thrown if this section is missing or doesn't contain a match.
-- For convenience, both service ID and target URL are returned.
findTargetForServiceId :: ServiceId -> ThentosConfig -> Action DB (ServiceId, String)
findTargetForServiceId sid conf = do
    target <- case Map.lookup sid (getProxyConfigMap conf) of
            Just proxy -> return $ extractTargetUrl proxy
            Nothing    -> throwError $ ProxyNotConfiguredForService sid
    return (sid, target)

-- | Look up the service ID and target URL in the "proxy" section of the config.
-- An error is thrown if that section is missing.
findDefaultServiceIdAndTarget :: ThentosConfig -> Action DB (ServiceId, String)
findDefaultServiceIdAndTarget conf = do
    defaultProxy <- maybe (throwError MissingServiceHeader) return $
        Tagged <$> conf >>. (Proxy :: Proxy '["proxy"])
    sid <- return . ServiceId $ defaultProxy >>. (Proxy :: Proxy '["service_id"])
    let target = extractTargetUrl defaultProxy
    return (sid, target)

extractTargetUrl :: ProxyConfig -> String
extractTargetUrl proxy = cs $ exposeUrl http <> prefix
  where
    http :: HttpConfig = Tagged $ proxy >>. (Proxy :: Proxy '["http"])
    prefix :: ST       = fromMaybe "" $ proxy >>. (Proxy :: Proxy '["url_prefix"])

-- | Create headers identifying the user and their groups.
-- Returns an empty list in case of an anonymous request.
createCustomHeaders :: RenderHeaderFun -> Maybe ThentosSessionToken -> ServiceId
                    -> Action DB T.RequestHeaders
createCustomHeaders _ Nothing _                    = return []
createCustomHeaders renderHeaderFun (Just tok) sid = do
    (uid, user) :: (UserId, User)
        <- do
            session <- lookupThentosSession tok
            case session ^. thSessAgent of
                UserA uid  -> lookupUser uid
                ServiceA servId -> throwError $ NeedUserA tok servId

    groups :: [Group] <- userGroups uid sid

    return [ (renderHeaderFun ThentosHeaderUser, cs . fromUserName $ user ^. userName)
           , (renderHeaderFun ThentosHeaderGroups, cs $ show groups)
           ]

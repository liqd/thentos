{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Thentos.Backend.Api.Proxy where

import Control.Exception (SomeException)
import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Data.Configifier (Tagged(Tagged), (>>.))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (SBS, cs)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Network.HTTP.ReverseProxy
import Network.HTTP.Types (status500)
import Servant.API (Raw)
import Servant.Server.Internal.ServantErr (responseServantErr)
import Servant.Server (Server, HasServer(..), ServantErr)
import System.Log.Logger (Priority(DEBUG, WARNING))
import System.Log.Missing (logger)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BSC
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

serviceProxy :: (Show e, Typeable e) =>
      C.Manager -> ProxyAdapter e -> ActionState -> Server ServiceProxy
serviceProxy manager adapter state
    = waiProxyTo (reverseProxyHandler adapter state)
                 err500onExc
                 manager

-- | Proxy or respond based on request headers.
reverseProxyHandler :: (Show e, Typeable e) =>
      ProxyAdapter e -> ActionState -> S.Request -> IO WaiProxyResponse
reverseProxyHandler adapter state req = do
    eRqMod <- runActionE () state $ getRqMod adapter req
    case fst eRqMod of
        Right (RqMod uri headers) -> do
          let proxyDest = ProxyDest { pdHost = cs $ proxyHost uri
                                    , pdPort = proxyPort uri }

          let pReq = prepareReq adapter headers (proxyPath uri) req
          return $ WPRModifiedRequest pReq proxyDest
        Left e -> WPRResponse . responseServantErr <$> renderError adapter e

-- | Allows adapting a proxy for a specific use case.
data ProxyAdapter e = ProxyAdapter
  { renderHeader     :: RenderHeaderFun
  , renderUserAction :: UserId -> User -> Action e () SBS
  , renderError      :: ActionError e -> IO ServantErr
  }

defaultProxyAdapter :: ProxyAdapter Void
defaultProxyAdapter = ProxyAdapter
  { renderHeader     = renderThentosHeaderName
  , renderUserAction = defaultRenderUserAction
  , renderError      = baseActionErrorToServantErr
  }

-- | Render the user by showing their name.
defaultRenderUserAction :: UserId -> User -> Action e () SBS
defaultRenderUserAction _ user = pure . cs . fromUserName $ user ^. userName

prepareReq :: ProxyAdapter e -> T.RequestHeaders -> BSC.ByteString -> S.Request -> S.Request
prepareReq adapter proxyHdrs pathPrefix req
    = req { S.requestHeaders = proxyHdrs <> newHdrs
          , S.rawPathInfo = newPath
          }
    where
        newHdrs = clearCustomHeaders (renderHeader adapter) (S.requestHeaders req)
        dropLeading = BSC.dropWhile (== '/')
        pathPrefix' = BSC.reverse . dropLeading $ BSC.reverse pathPrefix
        newPath = BSC.concat [ pathPrefix', "/", dropLeading $ S.rawPathInfo req]

-- | Request modifier that contains all information that is needed to
-- alter and forward an incoming request.
data RqMod = RqMod ProxyUri T.RequestHeaders
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
-- The first parameter allows adapting a proxy for a specific use case.
-- To get the default behavior, use 'defaultProxyAdapter'.
getRqMod :: ProxyAdapter e -> S.Request -> Action e () RqMod
getRqMod adapter req = do
    thentosConfig <- getConfig'P
    let mTok = lookupThentosHeaderSession (renderHeader adapter) req

    (sid, target) <- case lookupThentosHeaderService (renderHeader adapter) req of
        Just s  -> findTargetForServiceId s thentosConfig
        Nothing -> findDefaultServiceIdAndTarget thentosConfig

    hdrs <- createCustomHeaders adapter mTok sid
    let rqMod = RqMod target hdrs
    logger'P DEBUG $ concat
        ["forwarding proxy request ", cs showReqInfo, " with modifier: ", show rqMod]
    return rqMod
  where
    showReqInfo = BSC.concat [S.requestMethod req, " ", S.rawPathInfo req, S.rawQueryString req]

-- | Look up the target URL for requests based on the given service ID. This requires a "proxies"
-- section in the config. An error is thrown if this section is missing or doesn't contain a match.
-- For convenience, both service ID and target URL are returned.
findTargetForServiceId ::
    ServiceId -> ThentosConfig -> Action e () (ServiceId, ProxyUri)
findTargetForServiceId sid conf = do
    target <- case Map.lookup sid (getProxyConfigMap conf) of
            Just proxy -> return $ extractTargetUrl proxy
            Nothing    -> throwError $ ProxyNotConfiguredForService sid
    return (sid, target)

-- | Look up the service ID and target URL in the "proxy" section of the config.
-- An error is thrown if that section is missing.
findDefaultServiceIdAndTarget :: ThentosConfig -> Action e () (ServiceId, ProxyUri)
findDefaultServiceIdAndTarget conf = do
    defaultProxy <- maybe (throwError $ MissingServiceHeader) return $
        Tagged <$> conf >>. (Proxy :: Proxy '["proxy"])
    sid <- return . ServiceId $ defaultProxy >>. (Proxy :: Proxy '["service_id"])
    return (sid, extractTargetUrl defaultProxy)

-- | Create headers identifying the user and their groups.
-- Returns an empty list in case of an anonymous request.
createCustomHeaders ::
    ProxyAdapter e -> Maybe ThentosSessionToken -> ServiceId -> Action e () T.RequestHeaders
createCustomHeaders _ Nothing _         = return []
createCustomHeaders adapter (Just tok) _sid = do
    (uid, user) <- validateThentosUserSession tok
    accessRightsByAgent'P (UserA uid) >>= grantAccessRights'P
    renderedUser <- renderUserAction adapter uid user
    -- FIXME We may want to sent a persona's groups to the service (personaGroups action), but
    -- currently the Proxy doesn't know about personas and it's unclear whether/how services
    -- will use that info anyway
    --groups <- userGroups uid sid
    return [ (renderHeader adapter ThentosHeaderUser, renderedUser)
    --       , (renderHeader adapter ThentosHeaderGroups, cs $ show groups)
           ]

-- | Throw an Internal Server Error if the proxied app is unreachable.
err500onExc :: SomeException -> S.Application
err500onExc exc _ sendResponse = do
    logger WARNING $ "Couldn't call proxied app: " <> show exc
    sendResponse $ S.responseLBS
        status500 [contentTypeJsonHeader] (encode $ ErrorMessage "internal error")

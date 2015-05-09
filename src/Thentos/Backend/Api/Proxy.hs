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

module Thentos.Backend.Api.Proxy (ServiceProxy, serviceProxy) where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (lift)
import Control.Monad.Trans.Either (EitherT(EitherT), runEitherT, left)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Data.CaseInsensitive (foldedCase)
import Data.Configifier (Tagged(Tagged), (>>.))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, LBS, cs)
import Servant.API (Raw)
import Servant.Server.Internal (Server)
import System.Log.Logger (Priority(DEBUG))

import qualified Data.ByteString as SBS
import qualified Data.Map as Map
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types.Status as T
import qualified Network.HTTP.Types.Header as T
import qualified Network.Wai as S

import System.Log.Missing (logger)
import Thentos.Action
import Thentos.Action.Core
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Types


type ServiceProxy = Raw

serviceProxy :: PushActionSubRoute (Server ServiceProxy)
serviceProxy req cont = catchProxy cont $ do
    rqMod <- getRqMod req
    _ $ C.withManager C.defaultManagerSettings $ \ manager ->
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
getRqMod :: S.Request -> Action RqMod
getRqMod req = do
    thentosConfig <- getConfig'P

    prxCfg :: Map.Map ServiceId HttpProxyConfig
        <- maybe (throwError ProxyNotAvailable) return $ getProxyConfigMap thentosConfig

    (uid, user) :: (UserId, User)
        <- do
            tok <- ThentosSessionToken <$> maybe (throwError NoSuchThentosSession) return
                  (lookupRequestHeader req ThentosHeaderSession)
            session <- lookupThentosSession tok
            case session ^. thSessAgent of
                UserA uid  -> lookupUser uid
                ServiceA sid -> throwError $ NeedUserA tok sid

    sid :: ServiceId
        <- case lookupRequestHeader req ThentosHeaderService of
               Just s  -> return $ ServiceId s
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


-- | This is a work-around for the fact that we can't write an
-- instance for 'Application'.  See FIXME in the corresponding
-- 'PushActionC' instance.
--
-- (There is some redundancy between here and 'catchAction'.  Not sure
-- if we should spend time cleaning that up, or if we should wait for
-- something to happen to 'Raw' that cleans up this function
-- automatically.)
catchProxy :: forall r . (r ~ S.ResponseReceived) => (S.Response -> IO r) -> Action r -> Action r
catchProxy cont action =
    ReaderT $ \ state ->
        EitherT $ do
            outcome <- _  -- runEitherT $ action `runReaderT` state
            case outcome of
                Left e -> showThentosError e >>= \ (status, msg) -> Right <$>
                              cont (S.responseLBS (T.Status status (cs msg)) [] (cs msg))
                Right r -> return $ Right r

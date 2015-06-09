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
import Servant.Server (ServerT, Server, serve, enter)
import Servant.Server.Internal (ServerT, Server)
import System.Log.Logger (Priority(DEBUG, INFO))

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

import Control.Monad.Trans.Either (EitherT(EitherT), eitherT)
import LIO.Core (MonadLIO, LIO, LIOState(LIOState), liftLIO, evalLIO, setClearanceP)
import LIO.DCLabel (CNF, ToCNF, DCLabel, (%%), toCNF, cFalse)
import LIO.TCB (ioTCB)

type ServiceProxy = Raw

runProxy :: HttpConfig -> ActionState DB -> IO ()
runProxy cfg asg = do
    logger INFO $ "running rest api Thentos.Backend.Api.Simple on " ++ show (bindUrl cfg) ++ "."
    runWarpWithCfg cfg $ serveApp asg

serveApp :: ActionState DB -> S.Application
serveApp state = serve api (serviceProxy state)

api :: Proxy ServiceProxy
api = Proxy

serviceProxy :: ActionState DB -> Server ServiceProxy
serviceProxy state req cont = do
    eRqMod <- runActionE state $ getRqMod req
    case eRqMod of
        Right rqMod -> do
            C.withManager C.defaultManagerSettings $ \ manager ->
                prepareReq rqMod req >>=
                (`C.httpLbs` manager) >>=
                cont . prepareResp
        Left e -> undefined
            -- TODO: what do we want to do here? under which circumstances
            -- can we actually get here?

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
            tok <- ThentosSessionToken <$> maybe (throwError NoSuchThentosSession) return
                  (lookupRequestHeader req "X-Thentos-Session")
            session <- lookupThentosSession tok
            case session ^. thSessAgent of
                UserA uid  -> lookupUser uid
                ServiceA sid -> throwError $ NeedUserA tok sid

    sid :: ServiceId
        <- case lookupRequestHeader req "X-Thentos-Service" of
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

lookupRequestHeader :: S.Request -> T.HeaderName -> Maybe ST
lookupRequestHeader req name = cs <$> lookup name (S.requestHeaders req)
{-
--catchProxy :: forall r . (r ~ S.ResponseReceived) => (S.Response -> LIO DCLabel r) -> Action DB r -> Action DB r
catchProxy cont (Action action) =
    Action $ ReaderT $ \ state ->
        EitherT $ do
            outcome <- runEitherT $ action `runReaderT` state
            case outcome of
                --Left e -> (error "showThentosError" :: (Int, String)) >>= \ (status, msg) -> Right <$>
                Left e -> ioTCB $ return (500, "things went wrong!" :: String) >>= \ (status, msg) -> Right <$>
                              cont (S.responseLBS (T.Status status (cs msg)) [] (cs msg))
                Right r -> return $ Right r
  --where
  --  inner :: IO (Either ThentosError r)
  --  inner = (`evalLIO` LIOState dcBottom dcBottom)
  --        . eitherT (return . Left) (return . Right)
  --        $ fromAction action `runReaderT` state

-}
{-
-- | This is a work-around for the fact that we can't write an
-- instance for 'Application'.  See FIXME in the corresponding
-- 'PushActionC' instance.
--
-- (There is some redundancy between here and 'catchAction'.  Not sure
-- if we should spend time cleaning that up, or if we should wait for
-- something to happen to 'Raw' that cleans up this function
-- automatically.)
catchProxy :: forall r db . (r ~ S.ResponseReceived) => (S.Response -> IO r) -> Action db r -> Action db r
catchProxy cont action =
    ReaderT $ \ state ->
        EitherT $ do
            outcome <- _  -- runEitherT $ action `runReaderT` state
            case outcome of
                --Left e -> showThentosError e >>= \ (status, msg) -> Right <$>
                Left e -> _ >>= \ (status, msg) -> Right <$>
                              cont (S.responseLBS (T.Status status (cs msg)) [] (cs msg))
                Right r -> return $ Right r
-}

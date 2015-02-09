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

{-# OPTIONS  #-}

module Backend.Api.Proxy where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (lift)
import Control.Monad.Trans.Either (EitherT(EitherT), runEitherT, left)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT, ask)
import Data.CaseInsensitive (foldedCase)
import Data.Monoid ((<>))
import Data.String.Conversions (LBS, cs)
import Servant.API (Raw)
import Servant.Server.Internal (Server)
import System.Log.Logger (Priority(DEBUG))
import Text.Printf (printf)

import qualified Data.ByteString as SBS
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types.Status as T
import qualified Network.HTTP.Types.Header as T
import qualified Network.Wai as S

import Api
import Backend.Core
import DB
import Doc ()
import Types
import Config
import System.Log.Missing (logger)


type ServiceProxy = Raw

serviceProxy :: PushActionSubRoute (Server ServiceProxy)
serviceProxy req cont = catchProxy cont $ do
    RqMod (ProxyConfig target) proxyHdrs <- getRqMod req
    liftIO $ C.withManager C.defaultManagerSettings $ \ manager ->
        prepareReq target req proxyHdrs >>=
        (`C.httpLbs` manager) >>=
        cont . prepareResp

prepareReq :: String -> S.Request -> T.RequestHeaders -> IO C.Request
prepareReq target req proxyHdrs = do
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
data RqMod = RqMod ProxyConfig T.RequestHeaders
  deriving (Eq, Show)

-- | Extract proxy config from thentos config.  Look up session from
-- the token provided in the request header @X-Thentos-Session@ and
-- fill headers @X-Thentos-User@, @X-Thentos-Groups@.  If
-- 'proxyConfig' is 'Nothing' or an invalid or inactive session token
-- is provided, throw an error.
getRqMod :: S.Request -> Action r RqMod
getRqMod req = do
    ((_, _, thentosConfig), _) <- ask
    case proxyConfig thentosConfig of
        Nothing -> lift $ left ProxyNotAvailable
        Just prxCfg -> RqMod prxCfg <$> do
            (_, session) <- maybe (lift $ left NoSuchSession) (bumpSession . SessionToken) $
                lookupRequestHeader req "X-Thentos-Session"
            (_, user) <- queryAction $ LookupUser (session ^. sessionUser)

            let newHdrs =
                    ("X-Thentos-User", cs . fromUserName $ user ^. userName) :
                    ("X-Thentos-Groups", cs . show $ user ^. userGroups) :
                    []

            logger DEBUG $ printf "forwarding proxy request with extra headers:: %s." (show newHdrs)
            return newHdrs

-- | This is a work-around for the fact that we can't write an
-- instance for 'Application'.  See FIXME in the corresponding
-- 'PushActionC' instance.
catchProxy :: forall r . (r ~ S.ResponseReceived) => (S.Response -> IO r) -> RestAction r -> RestAction r
catchProxy cont action =
    ReaderT $ \ state ->
        EitherT $ do
            outcome <- runEitherT $ action `runReaderT` state
            case outcome of
                Left e -> showDbError e >>= \ (status, msg) -> Right <$>
                              cont (S.responseLBS (T.Status status (cs msg)) [] (cs msg))
                Right r -> return $ Right r

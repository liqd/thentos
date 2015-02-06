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

import Control.Concurrent.MVar (MVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Crypto.Random (SystemRNG)
import Data.CaseInsensitive (CI)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (SBS, LBS, ST, cs)
import Data.Text.Encoding (decodeUtf8')
import Network.Wai (Application, requestHeaders)
import Network.Wai.Handler.Warp (run)
import Servant.API ((:<|>)((:<|>)), (:>), Get, Post, Put, Delete, Capture, ReqBody, Raw)
import Servant.Docs (HasDocs, docsFor, docs, markdown)
import Servant.Server.Internal (HasServer, Server, route)
import Servant.Server (serve)

import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types.Status as T
import qualified Network.Wai as S

import Api
import Backend.Core
import DB
import Doc ()
import Types
import Config


type ProxyTest = Raw

proxyTest :: PushActionSubRoute (Server ProxyTest)
proxyTest req cont = do
    ((_, _, thentosConfig), _) <- ask
    liftIO $ case proxyConfig thentosConfig of
        Nothing -> cont $ S.responseLBS (T.Status 404 "Thentos: proxying deactivated.") [] ""
        Just (ProxyConfig target) -> C.withManager C.defaultManagerSettings $ \ manager ->
            prepareReq target req >>=
            (`C.httpLbs` manager) >>=
            cont . prepareResp

prepareReq :: String -> S.Request -> IO C.Request
prepareReq target req = do
    body <- S.strictRequestBody req
    req' <- C.parseUrl $ target ++ (cs $ S.rawPathInfo req)
    return . C.setQueryString (S.queryString req) $ req'
        { C.method         = S.requestMethod req
        , C.checkStatus    = \ _ _ _ -> Nothing
        , C.requestBody    = C.RequestBodyLBS body
        , C.requestHeaders = S.requestHeaders req
        }

prepareResp :: C.Response LBS -> S.Response
prepareResp res = S.responseLBS (C.responseStatus res) (C.responseHeaders res) (C.responseBody res)

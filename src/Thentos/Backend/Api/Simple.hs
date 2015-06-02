{-# LANGUAGE DataKinds                                #-}
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

module Thentos.Backend.Api.Simple where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Servant.API ((:<|>)((:<|>)), (:>), addHeader, Header, Headers, Post, Put, Delete, Capture, ReqBody, JSON)
import Servant.Server (ServerT, Server, serve, enter)
import System.Log.Logger (Priority(INFO))

import qualified Servant.API

import System.Log.Missing (logger)
import Thentos.Action
import Thentos.Action.Core  -- FIXME: this shouldn't be here.  use only things from Thentos.Action!
import Thentos.Backend.Api.Auth
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Types

import qualified Thentos.Transaction as T  -- FIXME: this shouldn't be here.  use Thentos.Action instead!


-- * main

runApi :: HttpConfig -> ActionState DB -> IO ()
runApi cfg asg = do
    logger INFO $ "running rest api Thentos.Backend.Api.Simple on " ++ show (bindUrl cfg) ++ "."
    runWarpWithCfg cfg $ serveApi asg

serveApi :: ActionState DB -> Application
serveApi = serve (Proxy :: Proxy Api) . api

type Api = ThentosAssertHeaders :> ThentosAuth :> ThentosBasic

api :: ActionState DB -> Server Api
api actionState mTok = enter (enterAction actionState mTok) thentosBasic


-- * toy example (FIXME: remove this section once understood.)

type ThentosBasic =
       "get" :> Get '[JSON] Int
  :<|> "post" :> Post '[JSON] ()

thentosBasic :: ServerT ThentosBasic (Action DB)
thentosBasic =
       return (addHeader "0" 3)
  :<|> return ()

type Get ctyps val = Servant.API.Get ctyps
                     (Headers '[Header "X" String {- Header "Cache-Control" String, Header "Expires" String -}] val)

-- FIXME: this is all nice and well, but we want to actually do the 'addHeader' call once at the top of the
-- routing tree, and it should trickle down only into the Gets, not into the Posts.



{- ----------------------------------------------------------------------



-- * combinators

type ThentosBasic =
       "user" :> ThentosUser
  :<|> "service" :> ThentosService
  :<|> "thentos_session" :> ThentosThentosSession
  :<|> "service_session" :> ThentosServiceSession

thentosBasic :: ServerT ThentosBasic (Action DB)
thentosBasic =
       thentosUser
  :<|> thentosService
  :<|> thentosThentosSession
  :<|> thentosServiceSession


-- * user

type ThentosUser =
       ReqBody '[JSON] UserFormData :> Post '[JSON] UserId
  :<|> Capture "uid" UserId :> Delete '[JSON] ()
  :<|> Capture "uid" UserId :> "name" :> ReqBody '[JSON] UserName :> Put '[JSON] ()
  :<|> Capture "uid" UserId :> "name" :> Get '[JSON] UserName
  :<|> Capture "uid" UserId :> "email" :> ReqBody '[JSON] UserEmail :> Put '[JSON] ()
  :<|> Capture "uid" UserId :> "email" :> Get '[JSON] UserEmail
  :<|> Get '[JSON] [UserId]

thentosUser :: ServerT ThentosUser (Action DB)
thentosUser =
       (>>= update'P . T.AddUser) . makeUserFromFormData'P
  :<|> update'P . T.DeleteUser
  :<|> (\ uid name -> update'P $ T.UpdateUserField uid (T.UpdateUserFieldName name))
  :<|> (((^. userName) . snd) <$>) . query'P . T.LookupUser
  :<|> (\ uid email -> update'P $ T.UpdateUserField uid (T.UpdateUserFieldEmail email))
  :<|> (((^. userEmail) . snd) <$>) . query'P . T.LookupUser
  :<|> allUserIds


-- * service

type ThentosService =
       ReqBody '[JSON] (UserId, ServiceName, ServiceDescription) :> Post '[JSON] (ServiceId, ServiceKey)
           -- FIXME: it would be much nicer to infer the owner from
           -- the session token, but that requires changes to the
           -- various action monads we are kicking around all over the
           -- place.  coming up soon!

  :<|> Capture "sid" ServiceId :> Delete '[JSON] ()
  :<|> Get '[JSON] [ServiceId]

thentosService :: ServerT ThentosService (Action DB)
thentosService =
         (\ (uid, sn, sd) -> addService (UserA uid) sn sd)
    :<|> update'P . T.DeleteService
    :<|> query'P T.AllServiceIds


-- * session

type ThentosThentosSession =
       ReqBody '[JSON] (UserId, UserPass)      :> Post '[JSON] ThentosSessionToken
  :<|> ReqBody '[JSON] (ServiceId, ServiceKey) :> Post '[JSON] ThentosSessionToken
  :<|> ReqBody '[JSON] ThentosSessionToken     :> Get '[JSON] Bool
  :<|> ReqBody '[JSON] ThentosSessionToken     :> Delete '[JSON] ()

thentosThentosSession :: ServerT ThentosThentosSession (Action DB)
thentosThentosSession =
       uncurry startThentosSessionByUserId
  :<|> uncurry startThentosSessionByServiceId
  :<|> existsThentosSession
  :<|> endThentosSession


-- * service session

type ThentosServiceSession =
       ReqBody '[JSON] ServiceSessionToken :> Get '[JSON] Bool
  :<|> ReqBody '[JSON] ServiceSessionToken :> "meta" :> Get '[JSON] ServiceSessionMetadata
  :<|> ReqBody '[JSON] ServiceSessionToken :> Delete '[JSON] ()

thentosServiceSession :: ServerT ThentosServiceSession (Action DB)
thentosServiceSession =
       existsServiceSession
  :<|> getServiceSessionMetadata
  :<|> endServiceSession


-- * Cache control

-- Only needed for GET and HEAD requests, as other HTTP methods are
-- uncacheable by nature.

-- (FIXME: move this section to its own module.  it is not Simple-specific.)

-- | Disable caching of GET responses.
type Get ctyps val = Servant.API.Get ctyps
                     (Headers '[Header "X" String {- Header "Cache-Control" String, Header "Expires" String -}] val)

-- type Head ctyps val = ...

-}

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
{-# LANGUAGE PackageImports                           #-}

module Thentos.Backend.Api.Simple where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Data.Proxy (Proxy(Proxy))
import LIO.DCLabel (dcPublic)
import Network.Wai (Application)
import Servant.API ((:<|>)((:<|>)), (:>), Get, Post, Put, Delete, Capture, ReqBody, JSON)
import Servant.Server.Internal (Server)
import Servant.Server (ServerT, serve, enter)
import System.Log.Logger (Priority(INFO))

import System.Log.Missing (logger)
import Thentos.Action
import Thentos.Action.Core  -- FIXME: this shouldn't be here.  use only things from Thentos.Action!
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Types

import qualified Thentos.Transaction as T  -- FIXME: this shouldn't be here.  use Thentos.Action instead!


-- * main

runApi :: HttpConfig -> ActionState -> IO ()
runApi cfg asg = do
    logger INFO $ "running rest api Thentos.Backend.Api.Simple on " ++ show (bindUrl cfg) ++ "."
    runWarpWithCfg cfg $ serveApi asg

serveApi :: ActionState -> Application
serveApi = serve (Proxy :: Proxy Api) . api

type Api = ThentosAssertHeaders ThentosBasic

api :: ActionState -> Server Api
api asg = enter (enterAction dcPublic asg) thentosBasic


-- * combinators

type ThentosBasic =
       "user" :> ThentosUser
  :<|> "service" :> ThentosService
  :<|> "session" :> ThentosThentosSession
  :<|> "servicesession" :> ThentosServiceSession

thentosBasic :: ServerT ThentosBasic Action
thentosBasic =
       thentosUser
  :<|> thentosService
  :<|> thentosThentosSession
  :<|> thentosServiceSession


-- * authentication

{-

-- | Empty data type for triggering authentication.  If you have an
-- api type 'API', use like this: @ThentosAuth :> API@, then write a
-- route handler that takes 'Auth' as an extra argument.  'Auth' will
-- be parsed from the headers and injected into the @sublayout@
-- handler.
data ThentosAuth layout = ThentosAuth ActionState layout

instance ( PushActionC (Server sublayout)
         , HasServer sublayout
         ) => HasServer (ThentosAuth sublayout)
  where
    type Server (ThentosAuth sublayout) = ThentosAuth ((Action sublayout))

    route Proxy (ThentosAuth asg subserver) request respond =
        route (Proxy :: Proxy sublayout) (pushAction routingState subserver) request respond
      where
        routingState :: ActionState
        routingState = asg

-}


-- * user

type ThentosUser =
       ReqBody '[JSON] UserFormData :> Post '[JSON] UserId
  :<|> Capture "uid" UserId :> Delete '[JSON] ()
  :<|> Capture "uid" UserId :> "name" :> ReqBody '[JSON] UserName :> Put '[JSON] ()
  :<|> Capture "uid" UserId :> "name" :> Get '[JSON] UserName
  :<|> Capture "uid" UserId :> "email" :> ReqBody '[JSON] UserEmail :> Put '[JSON] ()
  :<|> Capture "uid" UserId :> "email" :> Get '[JSON] UserEmail
  :<|> Get '[JSON] [UserId]

thentosUser :: ServerT ThentosUser Action
thentosUser =
       (>>= update'P . T.AddUser) . makeUserFromFormData'P
  :<|> update'P . T.DeleteUser
  :<|> (\ uid name -> update'P $ T.UpdateUserField uid (T.UpdateUserFieldName name))
  :<|> (((^. userName) . snd) <$>) . query'P . T.LookupUser
  :<|> (\ uid email -> update'P $ T.UpdateUserField uid (T.UpdateUserFieldEmail email))
  :<|> (((^. userEmail) . snd) <$>) . query'P . T.LookupUser
  :<|> query'P T.AllUserIds


-- * service

type ThentosService =
       ReqBody '[JSON] (UserId, ServiceName, ServiceDescription) :> Post '[JSON] (ServiceId, ServiceKey)
           -- FIXME: it would be much nicer to infer the owner from
           -- the session token, but that requires changes to the
           -- various action monads we are kicking around all over the
           -- place.  coming up soon!

  :<|> Capture "sid" ServiceId :> Delete '[JSON] ()
  :<|> Get '[JSON] [ServiceId]

thentosService :: ServerT ThentosService Action
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

thentosThentosSession :: ServerT ThentosThentosSession Action
thentosThentosSession =
       uncurry startThentosSessionByUserId
  :<|> uncurry startThentosSessionByServiceId
  :<|> existsThentosSession
  :<|> endThentosSession


-- * service session

type ThentosServiceSession =
       Capture "token" ServiceSessionToken :> Get '[JSON] Bool
  :<|> Capture "token" ServiceSessionToken :> "meta" :> Get '[JSON] ServiceSessionMetadata
  :<|> Capture "token" ServiceSessionToken :> Delete '[JSON] ()

thentosServiceSession :: ServerT ThentosServiceSession Action
thentosServiceSession =
       existsServiceSession
  :<|> getServiceSessionMetadata
  :<|> endServiceSession

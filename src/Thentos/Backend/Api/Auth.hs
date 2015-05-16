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
{-# LANGUAGE ViewPatterns                             #-}

module Thentos.Backend.Api.Auth where

import Data.Proxy (Proxy(Proxy))
import Network.Wai (Request)
import Servant.API ((:<|>)((:<|>)))
import Servant.Server (HasServer, Server, ServerT, route)

import Thentos.Action.Core
import Thentos.Backend.Core
import Thentos.Types


data ThentosAuth sub = ThentosAuth sub

-- | FIXME: 'route' requires handlers of type 'Server', not 'ServerT' with arbitrary monad.
-- therefore, the naive approach we tried in this module does not work: we cannot push an 'Action'
-- down the routes and then just sequence it with the handler's 'Action', because we need to talk
-- about the default servant monad.
--
-- This could be fixed by changing the 'HasServer' class, but perhaps there is an easier way.
-- instead of pushing down an 'Action', we could push down something that contains the action in a
-- form that fits into the existing servant types.
--
-- The problem is that it's not enough to run two actions that have been processed by 'enter'
-- separately: We want to modify the 'LIO' state with the action we push down the routes, but the
-- second action would not share the 'LIO' state with the first if they would not be sequenced as
-- @Action@, but as @ServerT ServantErr IO@.
instance ( HasServer subApi
         , HasPartialServer (Action DB) (ServerT subApi (Action DB))
         ) => HasServer (ThentosAuth subApi)
  where
    type ServerT (ThentosAuth subApi) (Action DB) = ThentosAuth (ServerT subApi (Action DB))
    route Proxy _ {- (ThentosAuth sub) -} request respond = route (Proxy :: Proxy subApi) sub' request respond
      where
        sub' :: Server subApi
        sub' = undefined  -- pushPartial (setPrivsFromRequest request) sub

setPrivsFromRequest :: Request -> Action DB ()
setPrivsFromRequest request = case lookupThentosHeaderSession request of
    Just tok -> clearanceByThentosSession tok >>= setClearance
    Nothing -> return ()

class HasPartialServer m a where
    type PushPartial m a
    pushPartial :: m () -> PushPartial m a -> a

instance (HasServer (a -> b), HasPartialServer m b) => HasPartialServer m (a -> b) where
    type PushPartial m (a -> b) = a -> PushPartial m b
    pushPartial p f = pushPartial p . f

instance (HasPartialServer m a, HasPartialServer m b) => HasPartialServer m (a :<|> b) where
    type PushPartial m (a :<|> b) = PushPartial m a :<|> PushPartial m b
    pushPartial p (a :<|> b) = pushPartial p a :<|> pushPartial p b

instance HasPartialServer (Action db) (Action db a) where
    type PushPartial (Action db) (Action db a) = Action db a
    pushPartial p m = p >> m

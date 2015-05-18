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

-- | Authentication via 'ThentosSessionToken'.
--
-- A LESS RELEVANT OBSERVATION: It would be nice if we could provide this function:
--
-- >>> thentosAuth :: ActionState DB
-- >>>             -> ServerT api (Action DB)
-- >>>             -> Maybe ThentosSessionToken
-- >>>             -> Server api
-- >>> thentosAuth actionState _api mTok = enter (enterAction actionState mTok) _api
--
-- because then here we could write:
--
-- >>> api :: ActionState DB -> Server (ThentosAuth :> MyApi)
-- >>> api = (`thentosAuch` myApi)
--
-- But the signature of `thentosAuth` requires injectivity of `ServerT` (`api` needs to be inferred
-- from `ServerT api (Action DB)`).  ghc-7.12 may help (see
-- https://ghc.haskell.org/trac/ghc/wiki/InjectiveTypeFamilies), or it may not: Even if injective
-- type families are supported, `ServerT` may not be injective in some particular type that this
-- function is called with.
--
-- So instead, you will have to write something like this:
--
-- >>> api :: ActionState DB -> Server (ThentosAuth :> MyApi)
-- >>> api actionState mTok = enter (enterAction actionState mTok) myApi
module Thentos.Backend.Api.Auth where

import Data.Proxy (Proxy(Proxy))
import Servant.API ((:>))
import Servant.Server (HasServer, ServerT, route)

import Thentos.Backend.Core
import Thentos.Types


data ThentosAuth

instance HasServer sub => HasServer (ThentosAuth :> sub) where
  type ServerT (ThentosAuth :> sub) m = (Maybe ThentosSessionToken) -> ServerT sub m
  route Proxy sub request = route (Proxy :: Proxy sub) (sub $ lookupThentosHeaderSession request) request

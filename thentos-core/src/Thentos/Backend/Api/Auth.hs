{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Authentication via 'ThentosSessionToken'.
--
-- A LESS RELEVANT OBSERVATION: It would be nice if we could provide this function:
--
-- >>> thentosAuth :: ActionEnv
-- >>>             -> ServerT api (Action)
-- >>>             -> Maybe ThentosSessionToken
-- >>>             -> Server api
-- >>> thentosAuth actionEnv api mTok = enter (enterAction actionEnv mTok) api
--
-- because then here we could write:
--
-- >>> api :: ActionEnv -> Server (ThentosAuth :> MyApi)
-- >>> api = (`thentosAuth` myApi)
--
-- But the signature of `thentosAuth` requires injectivity of `ServerT` (`api` needs to be inferred
-- from `ServerT api (Action)`).  ghc-7.12 may help (see
-- https://ghc.haskell.org/trac/ghc/wiki/InjectiveTypeFamilies), or it may not: Even if injective
-- type families are supported, `ServerT` may not be injective in some particular type that this
-- function is called with.
--
-- So instead, you will have to write something like this:
--
-- >>> api :: ActionEnv -> Server (ThentosAuth :> MyApi)
-- >>> api actionEnv mTok = enter (enterAction actionEnv mTok) myApi
module Thentos.Backend.Api.Auth (module Thentos.Backend.Api.Auth.Types) where

import Control.Lens ((&), (<>~))
import Data.CaseInsensitive (foldedCase)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs)
import Network.Wai (remoteHost)
import Network.Socket (SockAddr)
import Servant.API ((:>))
import Servant.Server (HasServer, ServerT, route)
import Servant.Server.Internal (passToServer)
import Servant.Utils.Links (HasLink(MkLink, toLink))

import qualified Servant.Foreign as F

import Thentos.Backend.Api.Auth.Types
import Thentos.Backend.Core


instance HasServer sub context => HasServer (ThentosAuth :> sub) context where
  type ServerT (ThentosAuth :> sub) m = ThentosAuthCredentials -> ServerT sub m
  route Proxy context sub = route (Proxy :: Proxy sub) context $ passToServer sub go
    where
      go request =
        let mTok = lookupThentosHeaderSession renderThentosHeaderName request in
        let origin :: SockAddr = remoteHost request in
        ThentosAuthCredentials mTok origin

instance HasLink sub => HasLink (ThentosAuth :> sub) where
    type MkLink (ThentosAuth :> sub) = MkLink sub
    toLink _ = toLink (Proxy :: Proxy sub)

instance F.HasForeign F.NoTypes () sub => F.HasForeign F.NoTypes () (ThentosAuth :> sub) where
    type Foreign () (ThentosAuth :> sub) = F.Foreign () sub
    foreignFor plang Proxy Proxy req = F.foreignFor plang Proxy (Proxy :: Proxy sub) $ req
            & F.reqHeaders <>~ [F.HeaderArg (F.Arg headerName ())]
      where
        headerName = F.PathSegment . cs . foldedCase $ renderThentosHeaderName ThentosHeaderSession

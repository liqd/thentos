{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Servant.Session (SSession) where

import           Data.Proxy                      (Proxy (Proxy))
import qualified Data.Vault.Lazy                 as Vault
import           Network.Wai                     (vault)
import           Network.Wai.Session             (Session)
import           Servant.API                     ((:>))
import           Servant.Server.Internal         (HasServer, ServerT, route, passToServer)
import           Servant.Server.Internal.Router  (Router'(WithRequest))


-- | @SSession m k v@ represents a session storage with keys of type @k@,
-- values of type @v@, and operating under the monad @m@.
-- The underlying implementation uses the 'wai-session' package, and any
-- backend compatible with that package should work here too.
data SSession (m :: * -> *) (k :: *) (v :: *)

-- | 'HasServer' instance for 'SSession'.
instance (HasServer sublayout) => HasServer (SSession n k v :> sublayout) where
  type ServerT (SSession n k v :> sublayout) m
    = (Vault.Key (Session n k v) -> Maybe (Session n k v)) -> ServerT sublayout m
  route Proxy a = WithRequest $ \request -> route (Proxy :: Proxy sublayout)
        $ passToServer a (\key -> Vault.lookup key $ vault request)

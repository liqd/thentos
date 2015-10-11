{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Thentos.Frontend.Session where

import Data.Text (Text)
import Data.String (fromString)
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Data.String.Conversions (ST)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Wai (requestHeaders)
import Control.Monad.State
import Web.ClientSession
import Web.Cookie
import Servant


-- | Example:
--
-- >>> type SessionState = String
-- >>> type MyApi = Session SessionState :> "test" :> Get '[JSON] Text
data Session (sym :: Symbol) payload = Session payload
  deriving (Typeable, Eq, Show, Functor)

-- FIXME: is something like SessionPayload already in servant?  or is there an implicit error if decoding state fails?

data SessionPayload a = SessionPayload a | SessionParseError ST | SessionNothing
  deriving (Typeable, Eq, Show, Functor)

instance ( KnownSymbol sym
         , FromText payload
         , HasServer sublayout
         , MonadState (SessionPayload payload) sublayout
         )
      => HasServer (Session sym payload :> sublayout) where

  type ServerT (Session sym payload :> sublayout) m = ServerT sublayout m
  route = undefined


createSession :: (p ~ SessionPayload payload, MonadState p m) => payload -> m ()
createSession = error "createSession"

destroySession :: (p ~ SessionPayload payload, MonadState p m) => m ()
destroySession = undefined

getSession :: (p ~ SessionPayload payload, MonadState p m) => m p
getSession = undefined

modifySession :: (p ~ SessionPayload payload, MonadState p m) => (Maybe payload -> Maybe payload) -> m ()
modifySession = undefined

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
import Data.String.Conversions (SBS)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Wai (requestHeaders)
import Control.Monad.State
import Web.ClientSession
import Web.Cookie
import Servant
import Servant.Server.Internal


data Session

newtype Token = Token SBS

-- FIXME: is something like SessionPayload already in servant?  or is there an implicit error if decoding state fails?

-- data SessionPayload a = SessionPayload a | SessionParseError ST | SessionNothing
--   deriving (Typeable, Eq, Show, Functor)

instance ( HasServer sublayout
         )
      => HasServer (Session :> sublayout) where

  type ServerT (Session :> sublayout) m = Token -> ServerT sublayout m

  -- lookup Cookie header; if n/a, create fresh token and set it in response.
  route Proxy a = WithRequest $ \request ->
    route (Proxy :: Proxy sublayout) $ do
      case lookup "Cookie" (requestHeaders request) of
        Just tok -> passToServer a $ Token tok
        Nothing -> do
          passToServer a mkToken

mkToken :: Token
mkToken = Token "bla"

{-
createSession :: (p ~ SessionPayload payload, MonadState p m) => payload -> m ()
createSession = error "createSession"

destroySession :: (p ~ SessionPayload payload, MonadState p m) => m ()
destroySession = undefined

getSession :: (p ~ SessionPayload payload, MonadState p m) => m p
getSession = undefined

modifySession :: (p ~ SessionPayload payload, MonadState p m) => (Maybe payload -> Maybe payload) -> m ()
modifySession = undefined
-}

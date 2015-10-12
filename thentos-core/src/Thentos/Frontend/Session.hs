{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
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
import Network.Wai -- (requestHeaders)
import Control.Monad.State
-- import Web.ClientSession
-- import Web.Cookie
import Servant
import Servant.Server.Internal
import Data.Aeson
import GHC.Generics
import Data.String.Conversions


data Session

newtype Token = Token String
  deriving (Eq, Show, Ord, Typeable, Generic, ToJSON, FromJSON)

-- FIXME: is something like SessionPayload already in servant?  or is there an implicit error if decoding state fails?

-- data SessionPayload a = SessionPayload a | SessionParseError ST | SessionNothing
--   deriving (Typeable, Eq, Show, Functor)

instance ( HasServer sublayout
         )
      => HasServer (Session :> sublayout) where

  type ServerT (Session :> sublayout) m = Token -> ServerT sublayout m

  -- lookup Cookie header; if n/a, create fresh token and set it in response.
  route Proxy a = WithRequest $ \request ->
    case lookup "Cookie" (requestHeaders request) of
      Just tok -> route (Proxy :: Proxy sublayout) . passToServer a . Token . cs $ tok
      Nothing ->
        let tok = mkToken in
        fmapRouter (injectToken tok) . route (Proxy :: Proxy sublayout) . passToServer a $ tok

mkToken :: Token  -- FIXME: IO Token
mkToken = Token "bla"


injectToken :: Token -> Response -> Response
injectToken = error "easy!"

-- PR for servant!!
fmapRouter :: (Response -> Response) -> Router -> Router
fmapRouter f (LeafRouter a) = LeafRouter $ \req cont -> a req (cont . (f <$>))
fmapRouter f (StaticRouter m) = StaticRouter (fmapRouter f <$> m)
fmapRouter f (DynamicRouter d) = DynamicRouter (fmapRouter f <$> d)
fmapRouter f (Choice r1 r2) = Choice (fmapRouter f r1) (fmapRouter f r2)




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


-- FIXME: EU bullshit
-- FIXME: other use case: only set cookie on login page (this comes after what snap currently does.)

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Thentos.Frontend.Session where

--import Data.ByteString (ByteString)
import Control.Monad
import Data.Typeable (Typeable)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs)
-- import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Wai -- (requestHeaders)
import Network.HTTP.Types (Header)
-- import Web.ClientSession
-- import Web.Cookie
import Servant.API ((:>))
import Servant.Server.Internal
import Data.Aeson
import GHC.Generics


data Session

newtype Token = Token String
  deriving (Eq, Show, Ord, Typeable, Generic, ToJSON, FromJSON)

tokenToHeader :: Token -> Header
tokenToHeader (Token t) = ("Set-Cookie", cs t)


instance ( HasServer sublayout
         )
      => HasServer (Session :> sublayout) where

  type ServerT (Session :> sublayout) m = Token -> ServerT sublayout m

  -- lookup Cookie header; if n/a, create fresh token and set it in response.
  route Proxy a = WithRequest $ \request ->
    case lookup "Cookie" (requestHeaders request) of
      Just tok -> route (Proxy :: Proxy sublayout) . passToServer a . Token . cs $ tok
      Nothing -> route (Proxy :: Proxy sublayout) $ (addCapture a' mkToken)
        where
          a' :: Delayed (Token -> ServerT sublayout)
          a' = (passToServer a tok :: Delayed (ServerT sublayout)) --- and now stick tok into the header of this


mkToken :: IO (RouteResult Token)
mkToken = return . Route . Token $ "bla"

injectToken :: Token -> Response -> IO Response
injectToken tok = return . mapResponseHeaders (tokenToHeader tok :)

-- PR for servant!!
fmapRouter :: (Response -> IO (Response, Token)) -> Router -> IO (Router, Token)
fmapRouter f (LeafRouter a) = LeafRouter $ \req cont -> a req (cont <=< _)
{-fmapRouter f (StaticRouter m) = StaticRouter (fmapRouter f <$> m)-}
{-fmapRouter f (DynamicRouter d) = DynamicRouter (fmapRouter f <$> d)-}
{-fmapRouter f (Choice r1 r2) = Choice (fmapRouter f r1) (fmapRouter f r2)-}
{-fmapRouter f (WithRequest g) = WithRequest (fmapRouter f . g)-}




-- FIXME: EU bullshit
-- FIXME: other use case: only set cookie on login page (this comes after what snap currently does.)

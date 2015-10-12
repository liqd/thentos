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

import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Data.Proxy (Proxy(Proxy))
import Data.Maybe (fromJust)
-- import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Wai -- (requestHeaders)
import Network.HTTP.Types (Header)
-- import Web.ClientSession
-- import Web.Cookie
import Servant.API ((:>))
import Servant.Server.Internal
import GHC.Generics


data Session
newtype Cookie = Cookie { unCookie :: ByteString }
  deriving (Eq, Show, Ord, Typeable, Generic)

cookieToHeader :: Cookie -> Header
cookieToHeader (Cookie t) = ("Set-Cookie", t)

instance (HasServer sublayout) => HasServer (Session :> sublayout) where

  type ServerT (Session :> sublayout) m = Cookie -> ServerT sublayout m

  route Proxy a = WithRequest $ \request -> route (Proxy :: Proxy sublayout)
        $ passToServer a (Cookie . fromJust $ lookup "Cookie" $ requestHeaders request)


mkCookie :: IO Cookie
mkCookie = return $ Cookie "bla"


midd :: Middleware
midd app req respond = case lookup "Cookie" (requestHeaders req) of
    Nothing -> do
        newCookie <- mkCookie
        app (req { requestHeaders = ("Cookie", unCookie newCookie):requestHeaders req}) (respond . injectCookie newCookie)
    Just _  -> app req respond


injectCookie :: Cookie -> Response -> Response
injectCookie tok = mapResponseHeaders (cookieToHeader tok :)





-- FIXME: EU bullshit
-- FIXME: other use case: only set cookie on login page (this comes after what snap currently does.)

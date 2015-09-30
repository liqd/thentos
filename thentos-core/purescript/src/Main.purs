module Main where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception
import Data.Array (zipWith, range, length)
import Data.Either
import Data.Foreign.Class
import Data.Foreign hiding (parseJSON)
import Data.JSON
import Data.Maybe
import Data.Tuple
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.Method
import Network.HTTP.MimeType
import Network.HTTP.MimeType.Common
import Network.HTTP.RequestHeader
import Prelude


defRq :: AffjaxRequest Unit
defRq = defaultRequest { headers = [ContentType applicationJSON, Accept applicationJSON] }

crash :: forall m err eff a b . (Show a) => a -> Aff (err :: EXCEPTION | eff) b
crash = liftEff <<< throwException <<< error <<< show

type Username = String
type Password = String
type ThentosSessionToken = String

data LoginRequestBody = LoginRequestBody
    { user :: Username
    , pass :: Password
    }

instance loginRequestBodyToJSON :: ToJSON LoginRequestBody where
  toJSON (LoginRequestBody { user: n, pass: p }) = object ["ldName" .= n, "ldPassword" .= p]

loginUser :: forall ajax err eff
     . Username -> Password
    -> Aff (ajax :: AJAX, err :: EXCEPTION | eff) (Array ThentosSessionToken)
loginUser username password = do
  let body = encode $ LoginRequestBody { user: username, pass: password }
  liftEff $ log "***"
  liftEff $ log body
  res <- affjax defRq
    { method = GET
    , url = "/user/login"
    , content = Just body
    }
  case eitherDecode res.response of
    Left e  -> crash e
    Right v -> return v

main = do
  log "Hello sailor!"

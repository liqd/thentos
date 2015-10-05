module Main where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE(), log)
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
import Network.HTTP.StatusCode (StatusCode(StatusCode))
import Prelude


defRq :: AffjaxRequest Unit
defRq = defaultRequest { headers = [ContentType applicationJSON, Accept applicationJSON] }

crash :: forall m err eff a b . (Show a) => a -> Aff (err :: EXCEPTION | eff) b
crash = liftEff <<< throwException <<< error <<< show

type Username = String
type Password = String
type ThentosSessionToken = String
type ThentosError = String

data LoginRequestBody = LoginRequestBody
    { user :: Username
    , pass :: Password
    }

instance loginRequestBodyToJSON :: ToJSON LoginRequestBody where
  toJSON (LoginRequestBody { user: n, pass: p }) = object ["ldName" .= n, "ldPassword" .= p]

loginUser :: forall ajax eff. Username -> Password
    -> Aff (ajax :: AJAX | eff) (Either ThentosError ThentosSessionToken)
loginUser username password = do
  let body = encode $ LoginRequestBody { user: username, pass: password }
  res <- affjax defRq
    { method = POST
    , url = "/user/login"
    , content = Just body
    }
  return if res.status == StatusCode 201
    then case eitherDecode res.response of
      Right v -> Right v
      Left e  -> Left $ "error decoding response: " ++ show (Tuple res.response e)
    else
      Left $ "server responsed with error: " ++ show res.status

main :: forall ajax err2 console. Eff (ajax :: AJAX, err :: EXCEPTION, console :: CONSOLE) Unit
main = do
  log "Hello sailor!"
  eTok <- launchAff (loginUser "god" "god")
  log $ show eTok

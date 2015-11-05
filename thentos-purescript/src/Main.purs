module Main where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE(), log, print)
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Random (RANDOM())
import Data.Array (zipWith, range, length)
import Data.Either
import Data.Foreign.Class
import Data.Foreign hiding (parseJSON)
import Data.Generic
import Data.Maybe
import Data.Tuple
import Halogen (HalogenEffects())
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.Method
import Network.HTTP.MimeType
import Network.HTTP.MimeType.Common
import Network.HTTP.RequestHeader
import Network.HTTP.StatusCode (StatusCode(StatusCode))
import Prelude

import qualified Error as Error
import qualified IFrameStressTest as IFrameStressTest


foreign import publish :: forall a. String -> String -> a -> forall eff. Eff eff Unit


defRq :: AffjaxRequest Unit
defRq = defaultRequest { headers = [ContentType applicationJSON, Accept applicationJSON] }

type Username = String
type Password = String
type ThentosSessionToken = String


data ThentosError = ConnectionError String | DecodeError String

derive instance genericThentosError :: Generic ThentosError
instance showThentosError :: Show ThentosError where
    show = gShow

data LoginRequestBody = LoginRequestBody
    { user :: Username
    , pass :: Password
    }

instance showLoginRequestBody :: Show LoginRequestBody where
    show (LoginRequestBody { user: n, pass: p }) = "{\"ldName\": " ++ show n ++ ", \"ldPassword\": " ++ show p ++ "}"

loginUser :: forall eff. Username -> Password
    -> Aff (ajax :: AJAX | eff) (Either ThentosError ThentosSessionToken)
loginUser username password = do
    let body :: String
        body = show $ LoginRequestBody { user: username, pass: password }

    res <- affjax defRq
        { method = POST
        , url = "/user/login"
        , content = Just body
        }
    return if res.status == StatusCode 201
            then Right res.response
            else Error.throwJS res.status

{-
        then case eitherDecode res.response of
            Right v -> Right v
            Left e  -> Left $ DecodeError $ "error decoding response: " ++ show (Tuple res.response e)
        else
            Left $ ConnectionError $ "server responsed with error: " ++ show res.status
-}


main :: forall eff.
    Eff (HalogenEffects (ajax :: AJAX, console :: CONSOLE, random :: RANDOM | eff)) Unit
main = do
    log "initializing thentos-purescript..."

    publish "Main" "counter" IFrameStressTest.counterMain
    publish "Main" "indicator" LoginIndicator.main

    IFrameStressTest.counterMain "body" (liftEff $ log "tick-handler")
    IFrameStressTest.counterMain "body" (return unit)
    LoginIndicator.main "body"

    runAff throwException print $ loginUser "god" "god"

    log "initialization of thentos-purescript complete!"

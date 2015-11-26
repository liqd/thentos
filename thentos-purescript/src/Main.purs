module Main where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE(), log, print)
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Random (RANDOM())
-- import Data.Array (zipWith, range, length)
import Data.Either
-- import Data.Foreign.Class
import Data.Foreign hiding (parseJSON)
import Data.Generic
import Data.Maybe
import Data.Tuple
import Halogen (HalogenEffects(), action)
import Halogen.Util (onLoad)
import Network.HTTP.Affjax
-- import Network.HTTP.Affjax.Request
-- import Network.HTTP.Affjax.Response
import Network.HTTP.Method
-- import Network.HTTP.MimeType
import Network.HTTP.MimeType.Common
import Network.HTTP.RequestHeader
import Network.HTTP.StatusCode (StatusCode(StatusCode))
import Prelude

import qualified Error as Error
import qualified Counter as Counter
import qualified IFramesDemo as IFramesDemo
import qualified Register as Register


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
    publish "Main" "counter" Counter.counterMain
    publish "Main" "counter_" Counter.counterRunner
    publish "Main" "tick" (action Counter.Tick)
    publish "Main" "clear" (action Counter.Clear)
    publish "Main" "indicator" LoginIndicator.main
    publish "IFrames" "main" IFramesDemo.main
    publish "Register" "main" Register.main
    publish "Register" "mainEl" Register.mainEl

    -- main0

    log "initialization of thentos-purescript complete!"

main0 :: forall eff.
    Eff (HalogenEffects (ajax :: AJAX, console :: CONSOLE, random :: RANDOM | eff)) Unit
main0 = do
    onLoad $ do
        Counter.counterMain "body" (liftEff $ log "tick-handler")
        runAff throwException (const (pure unit)) $ do
            (Tuple canceler driver) <- Counter.counterRunner "#id1" (return unit)
            later' 3000 $ driver (action Counter.Clear)
            later' 3000 $ do
                cancel canceler (error "jargh!")
                -- FIXME: dom-gc: killing the async job doesn't remove the widget's dom elements from the page, we need to do this manually!

        LoginIndicator.main "#id2"

    runAff throwException print $ loginUser "god" "god"

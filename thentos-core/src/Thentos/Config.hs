{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeOperators        #-}

module Thentos.Config
where

import Control.Applicative ((<|>))
import Control.Exception (throwIO, try)
import Data.Configifier
    ( (:>), (:*>)((:*>)), (:>:), (>>.)
    , configifyWithDefault, renderConfigFile, docs, defaultSources
    , ToConfigCode, ToConfig, Tagged(Tagged), TaggedM(TaggedM), MaybeO(..), Error
    )
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs, (<>))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.Mail.Mime (Address(Address))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (stdout)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler.Simple (formatter, fileHandler, streamHandler)
import System.Log.Logger (Priority(DEBUG, CRITICAL), removeAllHandlers, updateGlobalLogger,
                          setLevel, setHandlers)
import System.Log.Missing (loggerName, logger, Prio(..))
import Text.Show.Pretty (ppShow)

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as ST
import qualified Data.Text.IO as ST
import qualified Generics.Generic.Aeson as Aeson

import Thentos.Types


-- * config structure

type ThentosConfig = Tagged (ToConfigCode ThentosConfig')
type ThentosConfig' =
      Maybe ("frontend"     :> HttpConfig'           :>: "HTTP server for html forms.")
  :*> Maybe ("backend"      :> HttpConfig'           :>: "HTTP server for rest api.")
  :*> Maybe ("purescript"   :> ST                    :>: "File system location of frontend code")
  :*> Maybe ("proxy"        :> ProxyConfig'          :>: "The default proxied app.")
  :*> Maybe ("proxies"      :> [ProxyConfig']        :>: "A list of proxied apps.")
  :*>       ("smtp"         :> SmtpConfig'           :>: "Sending email.")
  :*>       ("database"     :> DatabaseConfig'       :>: "The database.")
  :*> Maybe ("default_user" :> DefaultUserConfig'    :>:
      "A user that is created if the user table is empty.")
  :*>       ("user_reg_expiration" :> Timeout        :>: "User registration expiration period")
  :*>       ("pw_reset_expiration" :> Timeout        :>:
      "Password registration token expiration period")
  :*>       ("email_change_expiration" :> Timeout    :>: "Email-change-token expiration period")
  :*>       ("captcha_expiration"      :> Timeout    :>: "Captcha expiration period")
  :*> Maybe ("gc_interval"             :> Timeout    :>: "Garbage collection interval")
  :*>       ("log"          :> LogConfig'            :>: "Logging")
  :*>       ("mail"         :> MailConfig'           :>: "Mail templates")
  :*>       ("a3-prefix"    :> ST                    :>: "URL prefix of the A3 api")

defaultThentosConfig :: ToConfig (ToConfigCode ThentosConfig') Maybe
defaultThentosConfig =
      NothingO
  :*> NothingO
  :*> NothingO
  :*> NothingO
  :*> NothingO
  :*> Just defaultSmtpConfig
  :*> Just defaultDatabaseConfig
  :*> NothingO
  :*> Just (fromHours 1)
  :*> Just (fromHours 1)
  :*> Just (fromHours 1)
  :*> Just (fromHours 1)
  :*> NothingO
  :*> Nothing
  :*> Just defaultMailConfig
  :*> Nothing

type HttpConfig = Tagged (ToConfigCode HttpConfig')
type HttpConfig' =
      Maybe ("bind_schema"   :> HttpSchema
      :>: "Http schema that the server experiences.  Differs from expose_schema e.g. when running behind nginx.")
  :*>       ("bind_host"     :> ST
      :>: "Host name that the server experiences.  Differs from expose_host e.g. when running behind nginx.")
  :*>       ("bind_port"     :> Int
      :>: "Host port that the server experiences.  Differs from expose_port e.g. when running behind nginx.")
  :*> Maybe ("expose_schema" :> HttpSchema
      :>: "Http schema that the client experiences.  Differs from bind_schema e.g. when running behind nginx.")
  :*> Maybe ("expose_host"   :> ST
      :>: "Host name that the client experiences.  Differs from bind_host e.g. when running behind nginx.")
  :*> Maybe ("expose_port"   :> Int
      :>: "Host port that the client experiences.  Differs from bind_port e.g. when running behind nginx.")

type ProxyConfig = Tagged (ToConfigCode ProxyConfig')
type ProxyConfig' =
            ("service_id" :> ST)
  :*>       ("endpoint"   :> ProxyUri)

type SmtpConfig = Tagged (ToConfigCode SmtpConfig')
type SmtpConfig' =
      Maybe ("sender_name"    :> ST)  -- FIXME: use more specific type 'Network.Mail.Mime.Address'
  :*>       ("sender_address" :> ST)
  :*>       ("sendmail_path"  :> ST)
  :*>       ("sendmail_args"  :> [ST])

defaultSmtpConfig :: ToConfig (ToConfigCode SmtpConfig') Maybe
defaultSmtpConfig =
      NothingO
  :*> Nothing
  :*> Just "/usr/sbin/sendmail"
  :*> Just ["-t"]

type DatabaseConfig = Tagged (ToConfigCode DatabaseConfig')
type DatabaseConfig' = "name" :> ST

defaultDatabaseConfig :: ToConfig (ToConfigCode DatabaseConfig') Maybe
defaultDatabaseConfig = Just "thentos"

type DefaultUserConfig = Tagged (ToConfigCode DefaultUserConfig')
type DefaultUserConfig' =
            ("name"     :> ST)  -- FIXME: use more specific type?
  :*>       ("password" :> ST)  -- FIXME: use more specific type?
  :*>       ("email"    :> UserEmail)
  :*> Maybe ("roles"    :> [Role])

type LogConfig = Tagged (ToConfigCode LogConfig')
type LogConfig' =
        ("path" :> ST)
    :*> ("level" :> Prio)

type MailConfig = Tagged (ToConfigCode MailConfig')
type MailConfig' =
        "account_verification" :> AccountVerificationConfig'
    :*> "user_exists"          :> UserExistsConfig'

type AccountVerificationConfig = Tagged (ToConfigCode AccountVerificationConfig')
type AccountVerificationConfig' =
      ("subject" :> ST)
  :*> ("body"    :> ST)

type UserExistsConfig = Tagged (ToConfigCode UserExistsConfig')
type UserExistsConfig' =
      ("subject" :> ST)
  :*> ("body"    :> ST)

defaultMailConfig :: ToConfig (ToConfigCode MailConfig') Maybe
defaultMailConfig =
        Just defaultAccountVerificationConfig
    :*> Just defaultUserExistsConfig

defaultAccountVerificationConfig :: ToConfig (ToConfigCode AccountVerificationConfig') Maybe
defaultAccountVerificationConfig =
      Just "Thentos account creation confirmation"
  :*> Just "Please go to {{activation_url}} to confirm your account."

defaultUserExistsConfig :: ToConfig (ToConfigCode UserExistsConfig') Maybe
defaultUserExistsConfig =
      Just "Thentos: Attempted Signup"
  :*> Just "Someone tried to sign up to Thentos with your email address."


-- * leaf types

data HttpSchema = Http | Https
  deriving (Eq, Ord, Enum, Bounded, Typeable, Generic)

instance Show HttpSchema where
    show Http = "http"
    show Https = "https"

instance Aeson.ToJSON HttpSchema where toJSON = Aeson.gtoJson
instance Aeson.FromJSON HttpSchema where parseJSON = Aeson.gparseJson


-- * driver

printConfigUsage :: IO ()
printConfigUsage = do
    -- ST.putStrLn $ docs (Proxy :: Proxy ThentosConfigDesc)
    ST.putStrLn $ docs (Proxy :: Proxy (ToConfigCode ThentosConfig'))


getConfig :: FilePath -> IO ThentosConfig
getConfig configFile = do
    sources <- defaultSources [configFile]
    logger DEBUG $ "config sources:\n" ++ ppShow sources

    result :: Either Error ThentosConfig <- try $ configifyWithDefault (TaggedM defaultThentosConfig) sources
    case result of
        Left e -> do
            logger CRITICAL $ "error parsing config: " ++ ppShow e
            throwIO e
        Right cfg -> do
            logger DEBUG $ "parsed config (yaml):\n" ++ cs (renderConfigFile cfg)
            logger DEBUG $ "parsed config (raw):\n" ++ ppShow cfg
            return cfg


-- ** helpers

-- this section contains code that works around missing features in
-- the supported leaf types in the config structure.  we hope it'll
-- get smaller over time.

getBackendConfig :: ThentosConfig -> HttpConfig
getBackendConfig cfg = (\(Just be) -> be) $ Tagged <$> cfg >>. (Proxy :: Proxy '["backend"])

getFrontendConfig :: ThentosConfig -> HttpConfig
getFrontendConfig cfg = (\(Just fe) -> fe) $ Tagged <$> cfg >>. (Proxy :: Proxy '["frontend"])

getProxyConfigMap :: ThentosConfig -> Map.Map ServiceId ProxyConfig
getProxyConfigMap cfg = fromMaybe Map.empty $ (Map.fromList . fmap (exposeKey . Tagged)) <$>
      cfg >>. (Proxy :: Proxy '["proxies"])
  where
    exposeKey :: ProxyConfig -> (ServiceId, ProxyConfig)
    exposeKey w = (ServiceId (w >>. (Proxy :: Proxy '["service_id"])), w)

bindUrl :: HttpConfig -> ST
bindUrl cfg = _renderUrl bs bh bp
  where
    bs = cfg >>. (Proxy :: Proxy '["bind_schema"])
    bh = cfg >>. (Proxy :: Proxy '["bind_host"])
    bp = cfg >>. (Proxy :: Proxy '["bind_port"])

exposeUrl :: HttpConfig -> ST
exposeUrl cfg = _renderUrl bs bh bp
  where
    bs = cfg >>. (Proxy :: Proxy '["expose_schema"]) <|> cfg >>. (Proxy :: Proxy '["bind_schema"])
    bh = fromMaybe (cfg >>. (Proxy :: Proxy '["bind_host"])) (cfg >>. (Proxy :: Proxy '["expose_host"]))
    bp = fromMaybe (cfg >>. (Proxy :: Proxy '["bind_port"])) (cfg >>. (Proxy :: Proxy '["expose_port"]))

extractTargetUrl :: ProxyConfig -> ProxyUri
extractTargetUrl proxy = proxy >>. (Proxy :: Proxy '["endpoint"])

_renderUrl :: Maybe HttpSchema -> ST -> Int -> ST
_renderUrl bs bh bp = (cs . show . fromMaybe Http $ bs) <> "://" <> bh <> ":" <> cs (show bp) <> "/"

buildEmailAddress :: SmtpConfig -> Address
buildEmailAddress cfg = Address (cfg >>. (Proxy :: Proxy '["sender_name"]))
                                (cfg >>. (Proxy :: Proxy '["sender_address"]))

getUserData :: DefaultUserConfig -> UserFormData
getUserData cfg = UserFormData
    (UserName  (cfg >>. (Proxy :: Proxy '["name"])))
    (UserPass  (cfg >>. (Proxy :: Proxy '["password"])))
    (cfg >>. (Proxy :: Proxy '["email"]))

getDefaultUser :: DefaultUserConfig -> (UserFormData, [Role])
getDefaultUser cfg = (getUserData cfg, fromMaybe [] (cfg >>. (Proxy :: Proxy '["roles"])))


-- * logging

-- FIXME: rewrite this

configLogger :: ST -> Prio -> IO ()
configLogger path prio = do
    let logfile = ST.unpack path
        loglevel = fromPrio prio
    removeAllHandlers
    createDirectoryIfMissing True $ takeDirectory logfile
    let fmt = simpleLogFormatter "$utcTime *$prio* [$pid][$tid] -- $msg"
    fHandler <- (\ h -> h { formatter = fmt }) <$> fileHandler logfile loglevel
    sHandler <- (\ h -> h { formatter = fmt }) <$> streamHandler stdout loglevel

    -- NOTE: `sHandler` was originally writing to stderr, but since thentos has more than one
    -- thread, this lead a lot of chaos where threads would log concurrently.  stdout is
    -- line-buffered and works better that way.

    updateGlobalLogger loggerName $
        System.Log.Logger.setLevel loglevel .
        setHandlers [sHandler, fHandler]

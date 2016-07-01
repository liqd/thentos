{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}

module Thentos.Config
    ( ThentosConfig
    , ThentosConfig'

    , HttpConfig
    , SmtpConfig
    , LogConfig
    , DatabaseConfig
    , EmailTemplates
    , EmailTemplate
    , DefaultUserConfig

    , getBackendConfig
    , getFrontendConfig
    , getProxyConfigMap
    , bindUrl
    , exposeUrl
    , extractTargetUrl
    , getDefaultUser
    , buildEmailAddress
    , signupLogger
    , defaultThentosConfig
    )
where

import Data.Configifier
    ( (:>), (:*>)((:*>)), (:>:), (>>.)
    , ToConfigCode, ToConfig, Tagged(Tagged), MaybeO(..)
    )
import Network.Mail.Mime (Address(Address))

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Generics.Generic.Aeson as Aeson

import Thentos.Prelude
import Thentos.Types


-- * config structure

type ThentosConfig = Tagged (ToConfigCode ThentosConfig')
type ThentosConfig' =
            ("root_path"    :> ST                    :>: "Directory to which all paths are relative (default: .)")
  :*> Maybe ("frontend"     :> HttpConfig'           :>: "HTTP server for html forms.")
  :*> Maybe ("backend"      :> HttpConfig'           :>: "HTTP server for rest api.")
  :*> Maybe ("allow_ips"    :> [ST]                  :>: "IP addresses for privileged access.")
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
  :*> Maybe ("signup_log"   :> ST                    :>: "Path of signup log file")
  :*>       ("email_templates" :> EmailTemplates'    :>: "Mail templates")
  :*> Maybe ("csrf_secret" :> ST                     :>: "The secret necessary to prevent CSRF attacks")

defaultThentosConfig :: ToConfig (ToConfigCode ThentosConfig') Maybe
defaultThentosConfig =
      Just "."
  :*> NothingO
  :*> NothingO
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
  :*> NothingO
  :*> Just defaultEmailTemplates
  :*> NothingO

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
  :*> Maybe ("groups"   :> [Group])

type LogConfig = Tagged (ToConfigCode LogConfig')
type LogConfig' =
        ("path" :> ST)
    :*> ("level" :> Prio)
    :*> ("stdout" :> Bool)

type EmailTemplates = Tagged (ToConfigCode EmailTemplates')
type EmailTemplates' =
        "account_verification" :> EmailTemplate'
    :*> "user_exists"          :> EmailTemplate'
    :*> "password_reset"       :> EmailTemplate'

type EmailTemplate = Tagged (ToConfigCode EmailTemplate')
type EmailTemplate' =
      ("subject" :> ST)
  :*> ("body"    :> ST)

defaultEmailTemplates :: ToConfig (ToConfigCode EmailTemplates') Maybe
defaultEmailTemplates =
        Nothing
    :*> Nothing
    :*> Nothing


-- * leaf types

data HttpSchema = Http | Https
  deriving (Eq, Ord, Enum, Bounded, Typeable, Generic)

instance Show HttpSchema where
    show Http = "http"
    show Https = "https"

instance Aeson.ToJSON HttpSchema where toJSON = Aeson.gtoJson
instance Aeson.FromJSON HttpSchema where parseJSON = Aeson.gparseJson


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
bindUrl cfg = renderUrl_ bs bh bp
  where
    bs = cfg >>. (Proxy :: Proxy '["bind_schema"])
    bh = cfg >>. (Proxy :: Proxy '["bind_host"])
    bp = cfg >>. (Proxy :: Proxy '["bind_port"])

exposeUrl :: HttpConfig -> ST
exposeUrl cfg = renderUrl_ bs bh bp
  where
    bs = cfg >>. (Proxy :: Proxy '["expose_schema"]) <|> cfg >>. (Proxy :: Proxy '["bind_schema"])
    bh = fromMaybe (cfg >>. (Proxy :: Proxy '["bind_host"])) (cfg >>. (Proxy :: Proxy '["expose_host"]))
    bp = fromMaybe (cfg >>. (Proxy :: Proxy '["bind_port"])) (cfg >>. (Proxy :: Proxy '["expose_port"]))

extractTargetUrl :: ProxyConfig -> ProxyUri
extractTargetUrl proxy = proxy >>. (Proxy :: Proxy '["endpoint"])

renderUrl_ :: Maybe HttpSchema -> ST -> Int -> ST
renderUrl_ mSchema host port = go (fromMaybe Http mSchema) port
  where
    go Http   80  = "http://" <> host <> "/"
    go Https  443 = "https://" <> host <> "/"
    go schema _   = cs (show schema) <> "://" <> host <> ":" <> cs (show port) <> "/"

buildEmailAddress :: SmtpConfig -> Address
buildEmailAddress cfg = Address (cfg >>. (Proxy :: Proxy '["sender_name"]))
                                (cfg >>. (Proxy :: Proxy '["sender_address"]))

getUserData :: DefaultUserConfig -> UserFormData
getUserData cfg = UserFormData
    (UserName  (cfg >>. (Proxy :: Proxy '["name"])))
    (UserPass  (cfg >>. (Proxy :: Proxy '["password"])))
    (cfg >>. (Proxy :: Proxy '["email"]))

getDefaultUser :: DefaultUserConfig -> (UserFormData, [Group])
getDefaultUser cfg = (getUserData cfg, fromMaybe [] (cfg >>. (Proxy :: Proxy '["groups"])))

signupLogger :: String
signupLogger = "signupLogger"

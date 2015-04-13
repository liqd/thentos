{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeOperators        #-}

module Thentos.Config
where

import Control.Applicative ((<$>), (<|>))
import Control.Exception (throwIO)
import Data.Configifier ((:>), (:*>)((:*>)), (:>:), (>>.))
import Data.Configifier (configify', renderConfigFile, docs)
import Data.Configifier (NoDesc, ToConfigCode, ToConfig, Source(..), Tagged(Tagged), TaggedM(TaggedM), Result, MaybeO(..))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs, (<>))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.Mail.Mime (Address(Address))
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment, getArgs)
import System.FilePath (takeDirectory)
import System.IO (stderr)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler.Simple (formatter, fileHandler, streamHandler)
import System.Log.Logger (Priority(DEBUG, CRITICAL), removeAllHandlers, updateGlobalLogger, setLevel, setHandlers)
import System.Log.Missing (loggerName, logger)
import Text.Show.Pretty (ppShow)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as SBS
import qualified Data.Map as Map
import qualified Data.Text.IO as ST
import qualified Generics.Generic.Aeson as Aeson

import Thentos.Types


-- * config structure

type ThentosConfig         = Tagged ThentosConfigUntagged
type ThentosConfigUntagged = NoDesc ThentosConfigDesc  -- ('NoDesc' will be deprecated in configifier-0.0.4)
type ThentosConfigDesc     = ToConfigCode ThentosConfig'

type ThentosConfig' =
            ("command"      :> Command)           :>: "One of 'run', runA3, 'showDB'."
  :*> Maybe ("frontend"     :> HttpConfig'        :>: "HTTP server for html forms.")
  :*> Maybe ("backend"      :> HttpConfig'        :>: "HTTP server for rest api.")
  :*> Maybe ("proxies"      :> [HttpProxyConfig'] :>: "HTTP server for tunneling requests to services.")
        -- FIXME: make proxies a map keyed by service ids
  :*>       ("smtp"         :> SmtpConfig')       :>: "Sending email."
  :*> Maybe ("default_user" :> DefaultUserConfig' :>: "A user that is created if the user table is empty.")

defaultThentosConfig :: ToConfig ThentosConfigUntagged Maybe
defaultThentosConfig =
      Just Run
  :*> NothingO
  :*> NothingO
  :*> NothingO
  :*> Just defaultSmtpConfig
  :*> NothingO

type HttpConfig = Tagged (NoDesc (ToConfigCode HttpConfig'))
type HttpConfig' =
      Maybe ("bind_schema"   :> HttpSchema)
  :*>       ("bind_host"     :> ST)
  :*>       ("bind_port"     :> Int)
  :*> Maybe ("expose_schema" :> HttpSchema)
  :*> Maybe ("expose_host"   :> ST)
  :*> Maybe ("expose_port"   :> Int)

type HttpProxyConfig = Tagged (NoDesc (ToConfigCode HttpProxyConfig'))
type HttpProxyConfig' =
            ("service_id" :> ST)
  :*>       ("http"       :> HttpConfig')
  :*> Maybe ("url_prefix" :> ST)

type SmtpConfig = Tagged (NoDesc (ToConfigCode SmtpConfig'))
type SmtpConfig' =
      Maybe ("sender_name"    :> ST)  -- FIXME: use more specific type 'Network.Mail.Mime.Address'
  :*>       ("sender_address" :> ST)
  :*>       ("sendmail_path"  :> ST)
  :*>       ("sendmail_args"  :> [ST])

defaultSmtpConfig :: ToConfig (NoDesc (ToConfigCode SmtpConfig')) Maybe
defaultSmtpConfig =
      NothingO
  :*> Nothing
  :*> Just "/usr/sbin/sendmail"
  :*> Just ["-t"]

type DefaultUserConfig = Tagged (NoDesc (ToConfigCode DefaultUserConfig'))
type DefaultUserConfig' =
            ("name"     :> ST)  -- FIXME: use more specific type?
  :*>       ("password" :> ST)  -- FIXME: use more specific type?
  :*>       ("email"    :> ST)  -- FIXME: use more specific type 'Network.Mail.Mime.Address'
  :*> Maybe ("roles"    :> [Role])


-- * leaf types

data Command = Run | RunA3 | ShowDB
  deriving (Eq, Ord, Show, Enum, Bounded, Typeable, Generic)

instance Aeson.ToJSON Command where toJSON = Aeson.gtoJson
instance Aeson.FromJSON Command where parseJSON = Aeson.gparseJson

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
    sources <- sequence
        [ ConfigFileYaml <$> SBS.readFile configFile
        , ShellEnv       <$> getEnvironment
        , CommandLine    <$> getArgs
        ]
    logger DEBUG $ "config sources:\n" ++ ppShow sources

    case configify' (TaggedM defaultThentosConfig) sources :: Result ThentosConfigUntagged of
        Left e -> do
            logger CRITICAL $ "error parsing config: " ++ show e
            throwIO e
        Right cfg -> do
            logger DEBUG $ "parsed config (yaml):\n" ++ cs (renderConfigFile cfg)
            logger DEBUG $ "parsed config (raw):\n" ++ ppShow cfg
            return cfg


-- ** helpers

-- this section contains code that works around missing features in
-- the supported leaf types in the config structure.  we hope it'll
-- get smaller over time.

getProxyConfigMap :: ThentosConfig -> Maybe (Map.Map ServiceId HttpProxyConfig)
getProxyConfigMap cfg = (Map.fromList . fmap (exposeKey . Tagged)) <$>
      cfg >>. (Proxy :: Proxy '["proxies"])
  where
    exposeKey :: HttpProxyConfig -> (ServiceId, HttpProxyConfig)
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

_renderUrl :: Maybe HttpSchema -> ST -> Int -> ST
_renderUrl bs bh bp = (cs . show . fromMaybe Http $ bs) <> "://" <> bh <> ":" <> cs (show bp) <> "/"

buildEmailAddress :: SmtpConfig -> Address
buildEmailAddress cfg = Address (cfg >>. (Proxy :: Proxy '["sender_name"]))
                                (cfg >>. (Proxy :: Proxy '["sender_address"]))

getUserData :: DefaultUserConfig -> UserFormData
getUserData cfg = UserFormData
    (UserName  (cfg >>. (Proxy :: Proxy '["name"])))
    (UserPass  (cfg >>. (Proxy :: Proxy '["password"])))
    (UserEmail (cfg >>. (Proxy :: Proxy '["email"])))

getDefaultUser :: DefaultUserConfig -> (UserFormData, [Role])
getDefaultUser cfg = (getUserData cfg, fromMaybe [] $ cfg >>. (Proxy :: Proxy '["roles"]))


-- * logging

-- FIXME: rewrite this, make properly configurable.

configLogger :: IO ()
configLogger = do
    let loglevel = DEBUG
        logfile = "./log/thentos.log"

    removeAllHandlers
    createDirectoryIfMissing True $ takeDirectory logfile
    let fmt = simpleLogFormatter "$utcTime *$prio* [$pid][$tid] -- $msg"
    fHandler <- (\ h -> h { formatter = fmt }) <$> fileHandler logfile loglevel
    sHandler <- (\ h -> h { formatter = fmt }) <$> streamHandler stderr loglevel

    updateGlobalLogger loggerName $
        System.Log.Logger.setLevel DEBUG .
        setHandlers [sHandler, fHandler]

{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}

-- | FIXME: we need to find a less verbose way to combine command line
-- arguments with data from config file.  See also: #1673, #1694.
module Thentos.Config
    ( getCommand
    , configLogger
    , Command(..)
    , ThentosConfig(..)
    , FrontendConfig(..)
    , BackendConfig(..)
    , ProxyConfig(..)
    , emptyThentosConfig
    ) where

import Control.Applicative (pure, (<$>), (<*>), (<|>), optional)
import Control.Monad (join)
import Data.Map (Map)
import Data.Monoid (Monoid(..), (<>))
import Network.Mail.Mime (Address(Address))
import Options.Applicative (command, info, progDesc, long, short, auto, option, flag, help)
import Options.Applicative (Parser, execParser, metavar, subparser)
import Safe (readDef)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (stderr)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler.Simple (formatter, fileHandler, streamHandler)
import System.Log.Logger (Priority(DEBUG), removeAllHandlers, updateGlobalLogger, setLevel, setHandlers)
import System.Log.Missing (loggerName)

import qualified Data.Configurator as Configurator
import qualified Data.Configurator.Types as Configurator
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map

import Thentos.Types


-- * the config type used by everyone else

data ThentosConfig = ThentosConfig
    { emailSender :: Address
    , frontendConfig :: Maybe FrontendConfig
    , backendConfig :: Maybe BackendConfig
    , proxyConfig :: Maybe ProxyConfig
    , defaultUser :: Maybe (UserFormData, [Role])
    }
  deriving (Eq, Show)

data BackendConfig = BackendConfig { backendPort :: Int }
  deriving (Eq, Show)

data FrontendConfig = FrontendConfig { frontendPort :: Int }
  deriving (Eq, Show)

data ProxyConfig = ProxyConfig { proxyTargets :: Map ServiceId String }
  deriving (Eq, Show)

emptyThentosConfig :: ThentosConfig
emptyThentosConfig = ThentosConfig (Address Nothing "") Nothing Nothing Nothing Nothing


-- * combining (partial) configurations from multiple sources

data ThentosConfigBuilder = ThentosConfigBuilder
    { bRunFrontend :: Maybe Bool
    , bRunBackend :: Maybe Bool
    , bEmailSender :: Maybe Address
    , bBackendConfig :: BackendConfigBuilder
    , bFrontendConfig :: FrontendConfigBuilder
    , bProxyConfig :: ProxyConfigBuilder
    , bDefaultUser :: Maybe (UserFormData, [Role])
    }

data BackendConfigBuilder = BackendConfigBuilder { bBackendPort :: Maybe Int }
data FrontendConfigBuilder = FrontendConfigBuilder { bFrontendPort :: Maybe Int }
data ProxyConfigBuilder = ProxyConfigBuilder
    { bProxyTarget :: Maybe (Map ServiceId String) }

instance Monoid BackendConfigBuilder where
    mempty = BackendConfigBuilder Nothing
    b1 `mappend` b2 =
        BackendConfigBuilder
            { bBackendPort = bBackendPort b1 <|> bBackendPort b2 }

instance Monoid FrontendConfigBuilder where
    mempty = FrontendConfigBuilder Nothing
    b1 `mappend` b2 =
        FrontendConfigBuilder
            { bFrontendPort = bFrontendPort b1 <|> bFrontendPort b2 }

instance Monoid ProxyConfigBuilder where
    mempty = ProxyConfigBuilder Nothing
    b1 `mappend` b2 =
        ProxyConfigBuilder
            { bProxyTarget = bProxyTarget b1 <|> bProxyTarget b2 }

instance Monoid ThentosConfigBuilder where
    mempty = ThentosConfigBuilder Nothing Nothing Nothing mempty mempty mempty Nothing
    b1 `mappend` b2 =
        ThentosConfigBuilder
            (bRunFrontend b1 <|> bRunFrontend b2)
            (bRunBackend b1 <|> bRunBackend b2)
            (bEmailSender b1 <|> bEmailSender b2)
            (bBackendConfig b1 <> bBackendConfig b2)
            (bFrontendConfig b1 <> bFrontendConfig b2)
            (bProxyConfig b1 <> bProxyConfig b2)
            (bDefaultUser b1 <|> bDefaultUser b2)

data ConfigError =
    FrontendConfigMissing
  | BackendConfigMissing
  | UnknownRoleForDefaultUser String
  | NoEmailSender
  deriving (Eq, Show)

finaliseConfig :: ThentosConfigBuilder -> Either ConfigError ThentosConfig
finaliseConfig builder =
    ThentosConfig
        <$> emailSenderConf
        <*> frontendConf
        <*> backendConf
        <*> proxyConf
        <*> defaultUserConf
  where
    emailSenderConf = maybe (Left NoEmailSender) Right $ bEmailSender builder
    backendConf = case (bRunBackend builder, finaliseBackendConfig $ bBackendConfig builder) of
        (Just True, Nothing) -> Left BackendConfigMissing
        (Just True, bConf) -> Right bConf
        _ -> Right Nothing
    frontendConf = case (bRunFrontend builder, finaliseFrontendConfig $ bFrontendConfig builder) of
        (Just True, Nothing) -> Left FrontendConfigMissing
        (Just True, fConf) -> Right fConf
        _ -> Right Nothing
    proxyConf = Right . finaliseProxyConfig $ bProxyConfig builder
    defaultUserConf = return $ bDefaultUser builder

finaliseFrontendConfig :: FrontendConfigBuilder -> Maybe FrontendConfig
finaliseFrontendConfig builder = FrontendConfig <$> bFrontendPort builder

finaliseBackendConfig :: BackendConfigBuilder -> Maybe BackendConfig
finaliseBackendConfig builder = BackendConfig <$> bBackendPort builder

finaliseProxyConfig :: ProxyConfigBuilder -> Maybe ProxyConfig
finaliseProxyConfig builder = ProxyConfig <$> bProxyTarget builder

finaliseCommand :: FilePath -> CommandBuilder -> IO (Either ConfigError Command)
finaliseCommand _ BShowDB = return $ Right ShowDB
finaliseCommand filePath (BRun cmdLineConfigBuilder) = do
    fileConfigBuilder <- parseConfigFile filePath
    let finalConfig = finaliseConfig $ cmdLineConfigBuilder <> fileConfigBuilder
    return $ Run <$> finalConfig
finaliseCommand filePath (BRunA3 cmdLineConfigBuilder) = do
    fileConfigBuilder <- parseConfigFile filePath
    let finalConfig = finaliseConfig $ cmdLineConfigBuilder <> fileConfigBuilder
    return $ RunA3 <$> finalConfig

getCommand :: FilePath -> IO (Either ConfigError Command)
getCommand filePath = do
    cmdLineBuilder <- parseCommandBuilder
    finaliseCommand filePath cmdLineBuilder


-- * command line parsing

parseCommandBuilder :: IO CommandBuilder
parseCommandBuilder = execParser opts
  where
    parser = subparser $ command "run" (info parseRun (progDesc "run")) <>
                         command "runa3" (info parseRunA3 (progDesc "run with a3 backend")) <>
                         command "showdb" (info (pure BShowDB) (progDesc "show"))
    opts = info parser mempty

data Command = Run ThentosConfig | RunA3 ThentosConfig | ShowDB
  deriving (Eq, Show)

data CommandBuilder =
    BRun ThentosConfigBuilder | BRunA3 ThentosConfigBuilder | BShowDB

parseRun :: Parser CommandBuilder
parseRun = BRun <$> parseThentosConfig

parseRunA3 :: Parser CommandBuilder
parseRunA3 = BRunA3 <$> parseThentosConfig

parseThentosConfig :: Parser ThentosConfigBuilder
parseThentosConfig =
    ThentosConfigBuilder <$>
        parseRunFrontend <*>
        parseRunBackend <*>
        pure Nothing <*>
        parseBackendConfigBuilder <*>
        parseFrontendConfigBuilder <*>
        parseProxyConfigBuilder <*>
        pure Nothing

parseBackendConfigBuilder :: Parser BackendConfigBuilder
parseBackendConfigBuilder =
    BackendConfigBuilder <$> optional parseBackendPort
  where
    parseBackendPort = option auto
        (long "backendport"
        <> metavar "backendPort"
        <> help "Port that the backend service listens on"
        )

parseFrontendConfigBuilder :: Parser FrontendConfigBuilder
parseFrontendConfigBuilder =
    FrontendConfigBuilder <$> optional parseFrontendPort
  where
    parseFrontendPort = option auto
        (long "frontendport"
        <> metavar "frontendPort"
        <> help "Port that the frontend service listens on"
        )

parseProxyConfigBuilder :: Parser ProxyConfigBuilder
parseProxyConfigBuilder =
    ProxyConfigBuilder <$> optional parseProxyTarget
  where
    parseProxyTarget = option auto
        (long "proxytargets"
        <> short 'f'
        <> metavar "proxyTarget"
        <> help "Where proxied requests will go"
        )

parseRunFrontend :: Parser (Maybe Bool)
parseRunFrontend = flag Nothing (Just True)
    (long "runfrontend"
    <> help "Run the frontend service"
    )

parseRunBackend :: Parser (Maybe Bool)
parseRunBackend = flag Nothing (Just True)
    (long "runbackend"
    <> help "Run the backend service"
    )


-- * config file parsing

parseConfigFile :: FilePath -> IO ThentosConfigBuilder
parseConfigFile filePath = do
    config <- Configurator.load [Configurator.Required filePath]
    argMap <- Configurator.getMap config

    let get :: Configurator.Configured a => Configurator.Name -> Maybe a
        get key = join $ Configurator.convert <$> HM.lookup key argMap

        getUser :: Maybe (UserFormData, [Role])
        getUser = do
            u <- UserFormData <$>
                (UserName <$> get "default_user.name") <*>
                (UserPass <$> get "default_user.password") <*>
                (UserEmail <$> get "default_user.email")
            rs :: [String] <- get "default_user.roles"
            let e r = error . show $ UnknownRoleForDefaultUser r  -- FIXME: error handling.
            return (u, map (\ r -> readDef (e r) r) rs)

        getEmail :: Maybe Address
        getEmail = Address (get "email_sender_name") <$> get "email_sender_address"

    return $ ThentosConfigBuilder
                (get "run_frontend")
                (get "run_backend")
                getEmail
                (BackendConfigBuilder $ get "backend_port")
                (FrontendConfigBuilder $ get "frontend_port")
                (ProxyConfigBuilder $ Map.fromList <$> get "proxy_targets")
                getUser


-- * logging

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

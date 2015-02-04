{-# LANGUAGE OverloadedStrings                        #-}

module Config
    ( getCommand
    , configLogger
    , Command(..)
    , ThentosConfig(..)
    , FrontendConfig(..)
    , BackendConfig(..)
    ) where

import Control.Applicative (pure, (<$>), (<*>), (<|>), optional)
import Control.Monad (join)
import Data.Monoid (Monoid(..), (<>))
import Options.Applicative (command, info, progDesc, long, auto, option, flag, help)
import Options.Applicative (Parser, execParser, metavar, subparser)
import System.IO (stderr)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler.Simple (formatter, fileHandler, streamHandler)
import System.Log.Logger (Priority(DEBUG), updateGlobalLogger, setLevel, setHandlers)
import System.Log.Missing (loggerName)

import qualified Data.Configurator as Configurator
import qualified Data.Configurator.Types as Configurator
import qualified Data.HashMap.Strict as HM


-- * the config type used by everyone else

data ThentosConfig = ThentosConfig
    { frontendConfig :: Maybe FrontendConfig
    , backendConfig :: Maybe BackendConfig
    }

data BackendConfig = BackendConfig { backendPort :: Int }
data FrontendConfig = FrontendConfig { frontendPort :: Int }


-- * combining (partial) configurations from multiple sources

data ThentosConfigBuilder = ThentosConfigBuilder
    { bRunFrontend :: Maybe Bool
    , bRunBackend :: Maybe Bool
    , bBackendConfig :: BackendConfigBuilder
    , bFrontendConfig :: FrontendConfigBuilder
    }

data BackendConfigBuilder = BackendConfigBuilder { bBackendPort :: Maybe Int }
data FrontendConfigBuilder = FrontendConfigBuilder { bFrontendPort :: Maybe Int }

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

instance Monoid ThentosConfigBuilder where
    mempty = ThentosConfigBuilder Nothing Nothing mempty mempty
    b1 `mappend` b2 =
        ThentosConfigBuilder
            (bRunFrontend b1 <|> bRunFrontend b2)
            (bRunBackend b1 <|> bRunBackend b2)
            (bBackendConfig b1 <> bBackendConfig b2)
            (bFrontendConfig b1 <> bFrontendConfig b2)

data ConfigError = FrontendError | BackendError

finaliseConfig :: ThentosConfigBuilder -> Either ConfigError ThentosConfig
finaliseConfig builder =
    ThentosConfig
        <$> frontendConf
        <*> backendConf
  where
    backendConf = case (bRunBackend builder, finaliseBackendConfig $ bBackendConfig builder) of
        (Just True, Nothing) -> Left BackendError
        (Just True, bConf) -> Right bConf
        _ -> Right Nothing
    frontendConf = case (bRunFrontend builder, finaliseFrontendConfig $ bFrontendConfig builder) of
        (Just True, Nothing) -> Left FrontendError
        (Just True, fConf) -> Right fConf
        _ -> Right Nothing

finaliseFrontendConfig :: FrontendConfigBuilder -> Maybe FrontendConfig
finaliseFrontendConfig builder = FrontendConfig <$> bFrontendPort builder

finaliseBackendConfig :: BackendConfigBuilder -> Maybe BackendConfig
finaliseBackendConfig builder = BackendConfig <$> bBackendPort builder

finaliseCommand :: FilePath -> CommandBuilder -> IO (Either ConfigError Command)
finaliseCommand _ BShowDB = return $ Right ShowDB
finaliseCommand _ BDocs = return $ Right Docs
finaliseCommand filePath (BRun cmdLineConfigBuilder) = do
    fileConfigBuilder <- parseConfigFile filePath
    let finalConfig = finaliseConfig $ cmdLineConfigBuilder <> fileConfigBuilder
    return $ Run <$> finalConfig

getCommand :: FilePath -> IO (Either ConfigError Command)
getCommand filePath = do
    cmdLineBuilder <- parseCommandBuilder
    finaliseCommand filePath cmdLineBuilder


-- * command line parsing

parseCommandBuilder :: IO CommandBuilder
parseCommandBuilder = execParser opts
  where
    parser = subparser $ command "run" (info parseRun (progDesc "run")) <>
                         command "docs" (info (pure BDocs) (progDesc "show")) <>
                         command "showdb" (info (pure BShowDB) (progDesc "show"))
    opts = info parser mempty

data Command = Run ThentosConfig | ShowDB | Docs

data CommandBuilder =
    BRun ThentosConfigBuilder | BShowDB | BDocs

parseRun :: Parser CommandBuilder
parseRun = BRun <$> parseServiceConfig

parseServiceConfig :: Parser ThentosConfigBuilder
parseServiceConfig =
    ThentosConfigBuilder <$>
        parseRunFrontend <*>
        parseRunBackend <*>
        parseBackendConfigBuilder <*>
        parseFrontendConfigBuilder

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
    let get key = join $ Configurator.convert <$> HM.lookup key argMap
    return $ ThentosConfigBuilder
                (get "run_frontend")
                (get "run_backend")
                (BackendConfigBuilder $ get "backend_port")
                (FrontendConfigBuilder $ get "frontend_port")


-- * logging

configLogger :: IO ()
configLogger = do
    let fmt = simpleLogFormatter "$utcTime *$prio* [$pid][$tid] -- $msg"
    fHandler <- (\ h -> h { formatter = fmt }) <$> fileHandler "./log/thentos.log" DEBUG
    sHandler <- (\ h -> h { formatter = fmt }) <$> streamHandler stderr DEBUG

    updateGlobalLogger loggerName $
        System.Log.Logger.setLevel DEBUG .
        setHandlers [sHandler, fHandler]

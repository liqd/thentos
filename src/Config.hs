{-# LANGUAGE OverloadedStrings                        #-}

module Config
    ( getCommandWithConfig
    , configLogger
    , Command(..)
    , ServiceConfig(..)
    , FrontendConfig(..)
    , BackendConfig(..)
    ) where

import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler.Simple (formatter, fileHandler, streamHandler)
import System.Log.Logger (Priority(DEBUG), updateGlobalLogger, setLevel, setHandlers)
import System.Log.Missing (loggerName)
import System.IO (stderr)

import Control.Applicative (pure, (<$>), (<*>), (<|>), optional)
import Control.Monad (join)
import qualified Data.Configurator as Configurator
import qualified Data.Configurator.Types as Configurator
import Data.Monoid (Monoid(..), (<>))
import Options.Applicative (Parser, execParser, info, str, metavar, argument,
    subparser, command, info, progDesc, long, auto, option, flag, help)

import qualified Data.HashMap.Strict as HM

parseConfigFile :: IO ServiceConfigBuilder
parseConfigFile = do
    config <- Configurator.load [Configurator.Required "devel.config"]
    argMap <- Configurator.getMap config
    let get key = join $ Configurator.convert <$> HM.lookup key argMap
    return $ ServiceConfigBuilder
                (get "run_frontend")
                (get "run_backend")
                (BackendConfigBuilder $ get "backend_port")
                (FrontendConfigBuilder $ get "frontend_port")

data ServiceConfig = ServiceConfig
    { frontendConfig :: Maybe FrontendConfig
    , backendConfig :: Maybe BackendConfig
    }

data ServiceConfigBuilder = ServiceConfigBuilder
    { bRunFrontend :: Maybe Bool
    , bRunBackend :: Maybe Bool
    , bBackendConfig :: BackendConfigBuilder
    , bFrontendConfig :: FrontendConfigBuilder
    }

data BackendConfig = BackendConfig { backendPort :: Int }
data FrontendConfig = FrontendConfig { frontendPort :: Int }

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

instance Monoid ServiceConfigBuilder where
    mempty = ServiceConfigBuilder Nothing Nothing mempty mempty
    b1 `mappend` b2 =
        ServiceConfigBuilder
            (bRunFrontend b1 <|> bRunFrontend b2)
            (bRunBackend b1 <|> bRunBackend b2)
            (bBackendConfig b1 <> bBackendConfig b2)
            (bFrontendConfig b1 <> bFrontendConfig b2)

data ConfigError = FrontendError | BackendError

finaliseConfig :: ServiceConfigBuilder -> Either ConfigError ServiceConfig
finaliseConfig builder =
    ServiceConfig
        <$> frontendConf
        <*> backendConf
  where
    backendConf =
        case (bRunBackend builder, finaliseBackendConfig $ bBackendConfig builder) of
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

finaliseCommand :: CommandBuilder -> IO (Either ConfigError Command)
finaliseCommand BShowDB = return $ Right ShowDB
finaliseCommand (BAddData s) = return . Right $ AddData s
finaliseCommand (BDocs) = return $ Right Docs
finaliseCommand (BRun cmdLineConfigBuilder) = do
    fileConfigBuilder <- parseConfigFile
    let finalConfig = finaliseConfig $ cmdLineConfigBuilder <> fileConfigBuilder
    return $ Run <$> finalConfig

getCommandWithConfig :: IO (Either ConfigError Command)
getCommandWithConfig = do
    cmdLineBuilder <- parseCommandBuilder
    finaliseCommand cmdLineBuilder
    

parseCommandBuilder :: IO CommandBuilder
parseCommandBuilder = execParser opts
  where
    parser = subparser $ command "run" (info parseRun (progDesc "run")) <>
                         command "showdb" (info parseShowDB (progDesc "show")) <>
                         command "add" (info parseAddData (progDesc "add"))
    opts = info parser mempty

-- FIXME: AddData should have a `User | Service` argument instead of string
data Command = Run ServiceConfig | ShowDB | AddData String | Docs

data CommandBuilder =
    BRun ServiceConfigBuilder | BShowDB | BAddData String | BDocs

parseRun :: Parser CommandBuilder
parseRun = BRun <$> parseServiceConfig

parseShowDB :: Parser CommandBuilder
parseShowDB = pure BShowDB

-- TODO: restrict DATAKIND to DummyUser and DummyService
parseAddData :: Parser CommandBuilder
parseAddData = BAddData <$> argument str (metavar "DATAKIND")

parseServiceConfig :: Parser ServiceConfigBuilder
parseServiceConfig =
    ServiceConfigBuilder <$>
        parseRunFrontend <*>
        parseRunBackend <*>
        parseBackendConfigBuilder <*>
        parseFrontendConfigBuilder

parseBackendConfigBuilder :: Parser BackendConfigBuilder
parseBackendConfigBuilder =
    BackendConfigBuilder <$> optional parseBackendPort
        
parseFrontendConfigBuilder :: Parser FrontendConfigBuilder
parseFrontendConfigBuilder = FrontendConfigBuilder <$> optional parseFrontendPort

parseBackendPort :: Parser Int
parseBackendPort = option auto
    (long "backendport"
    <> metavar "backendPort"
    <> help "Port that the backend service listens on"
    )

parseFrontendPort :: Parser Int
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

-- logging

configLogger :: IO ()
configLogger = do
    let fmt = simpleLogFormatter "$utcTime *$prio* [$pid][$tid] -- $msg"
    fHandler <- (\ h -> h { formatter = fmt }) <$> fileHandler "./log/thentos.log" DEBUG
    sHandler <- (\ h -> h { formatter = fmt }) <$> streamHandler stderr DEBUG

    updateGlobalLogger loggerName $
        System.Log.Logger.setLevel DEBUG .
        setHandlers [sHandler, fHandler]

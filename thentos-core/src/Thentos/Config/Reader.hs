{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -O0 #-}
module Thentos.Config.Reader where

import Data.Configifier
import System.Log.Logger
import System.Directory (createDirectoryIfMissing, setCurrentDirectory, canonicalizePath)
import System.FilePath (takeDirectory)
import System.IO (stdout)
import System.Log.Formatter (simpleLogFormatter, nullFormatter)
import System.Log.Handler.Simple (formatter, fileHandler, streamHandler)

import Thentos.Config

import qualified Data.Text as ST
import qualified Data.Text.IO as ST

import Thentos.Prelude

readConfig :: FilePath -> IO ThentosConfig
readConfig configFile = defaultSources' "THENTOS_" [configFile] >>= readConfigWithSources

readConfigWithSources :: [Source] -> IO ThentosConfig
readConfigWithSources sources = do
    logger DEBUG $ "config sources:\n" ++ ppShow sources

    result <- try $ configifyWithDefault (TaggedM defaultThentosConfig) sources
    case result of
        Left (e :: Error) -> do
            logger CRITICAL $ "error parsing config: " ++ ppShow e
            printConfigUsage
            throwIO e
        Right cfg -> do
            logger DEBUG $ "parsed config (yaml):\n" ++ cs (renderConfigFile cfg)
            logger DEBUG $ "parsed config (raw):\n" ++ ppShow cfg
            configLogger (Tagged $ cfg >>. (Proxy :: Proxy '["log"]))
            configSignupLogger (cfg >>. (Proxy :: Proxy '["signup_log"]))
            setRootPath (cfg >>. (Proxy :: Proxy '["root_path"]))
            return cfg


-- ** helpers

printConfigUsage :: IO ()
printConfigUsage = do
    ST.putStrLn $ docs (Proxy :: Proxy (ToConfigCode ThentosConfig'))

setRootPath :: ST -> IO ()
setRootPath (cs -> path) = do
    canonicalizePath path >>= logger INFO . ("Current working directory: " ++) . show
    setCurrentDirectory path


-- * logging

-- | Note: logging to stderr does not work very well together with multiple threads.  stdout is
-- line-buffered and works better that way.
--
-- FIXME: there should be a way to override an already-set log file with 'no log file' on e.g. the
-- command line.  (difficult with the current 'Maybe' solution; this may be a configifier patch.)
configLogger :: LogConfig -> IO ()
configLogger config = do
    let logfile = ST.unpack $ config >>. (Proxy :: Proxy '["path"])
        loglevel = fromPrio $ config >>. (Proxy :: Proxy '["level"])
        logstdout :: Bool = config >>. (Proxy :: Proxy '["stdout"])
    removeAllHandlers
    createDirectoryIfMissing True $ takeDirectory logfile

    let fmt = simpleLogFormatter "$utcTime *$prio* [$pid][$tid] -- $msg"
        mkHandler f = (\h -> h { formatter = fmt }) <$> f loglevel

    handlers <- sequence $ mkHandler <$> fileHandler logfile : [streamHandler stdout | logstdout]

    updateGlobalLogger loggerName $
        setLevel loglevel .
        setHandlers handlers

configSignupLogger :: Maybe ST -> IO ()
configSignupLogger Nothing = return ()
configSignupLogger (Just path) = do
    let logfile = ST.unpack path
    createDirectoryIfMissing True $ takeDirectory logfile
    handler <- fileHandler logfile DEBUG
    let handler' = handler { formatter = nullFormatter }
    updateGlobalLogger signupLogger (setHandlers [handler'])

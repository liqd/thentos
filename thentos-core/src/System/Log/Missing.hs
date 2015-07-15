{-# OPTIONS -fno-warn-orphans #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}


module System.Log.Missing
  ( logger
  , loggerName
  , announceAction
  )
where

import Control.Exception (bracket_)
import System.Log.Logger
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), Value(String))
import Data.Data (Typeable)
import Data.Text (toUpper, pack)

-- | 'logM' has two drawbacks: (1) It asks for a hierarchical logger
-- (aka component or module) name, but we don't want to bother with
-- that; (2) it lives in 'IO', not 'MonadIO m => m'.  'log' is defined
-- in "Prelude", that's why the slightly different name.
logger :: MonadIO m => Priority -> String -> m ()
logger prio msg = liftIO $ logM loggerName prio msg

loggerName :: String
loggerName = "Thentos"

announceAction :: String -> IO a -> IO a
announceAction msg = bracket_ (logger INFO msg) (logger INFO $ msg ++ ": [ok]")

deriving instance Typeable Priority

instance FromJSON Priority where
    parseJSON (String s) = case toUpper s of
        "DEBUG" -> return DEBUG
        "INFO" -> return INFO
        "NOTICE" -> return NOTICE
        "WARNING" -> return WARNING
        "ERROR" -> return ERROR
        "CRITICAL" -> return CRITICAL
        "ALERT" -> return ALERT
        "EMERGENCY" -> return EMERGENCY
        _ -> fail "not a valid log priority"
    parseJSON _ = fail "expected a string representing log priority"

instance ToJSON Priority where
    toJSON = String . pack . show

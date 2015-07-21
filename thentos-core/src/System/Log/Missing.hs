{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}


module System.Log.Missing
  ( logger
  , loggerName
  , announceAction
  , Prio(..)
  )
where

import Control.Applicative ((<$>))
import Control.Exception (bracket_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), Value(String))
import Data.Data (Typeable)
import Data.String.Conversions (cs)
import Data.Text (toUpper, pack)
import Safe (readMay)

import System.Log.Logger

-- | 'logM' has two drawbacks: (1) It asks for a hierarchical logger
-- (aka component or module) name, but we don't want to bother with
-- that; (2) it lives in 'IO', not 'MonadIO m => m'.  'log' is defined
-- in "Prelude", that's why the slightly different name.

newtype Prio = Prio { fromPrio :: Priority }
    deriving (Show, Eq)

logger :: MonadIO m => Priority -> String -> m ()
logger prio msg = liftIO $ logM loggerName prio msg

loggerName :: String
loggerName = "Thentos"

announceAction :: String -> IO a -> IO a
announceAction msg = bracket_ (logger INFO msg) (logger INFO $ msg ++ ": [ok]")

deriving instance Typeable Prio

instance FromJSON Prio where
    parseJSON (String s) = Prio <$> maybe (fail $ "not a valid log priority: " ++ cs s) return
                                          (readMay . cs . toUpper $ s)
    parseJSON _ = fail "expected a string representing log priority"

instance ToJSON Prio where
    toJSON = String . pack . show . fromPrio

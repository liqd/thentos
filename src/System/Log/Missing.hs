module System.Log.Missing
  ( logger
  , loggerName
  )
where

import System.Log.Logger
import Control.Monad.IO.Class (MonadIO, liftIO)

-- | 'logM' has two drawbacks: (1) It asks for a hierarchical logger
-- (aka component or module) name, but we don't want to bother with
-- that; (2) it lives in 'IO', not 'MonadIO m => m'.  'log' is defined
-- in "Prelude", that's why the slightly different name.
logger :: MonadIO m => Priority -> String -> m ()
logger prio msg = liftIO $ logM loggerName prio msg

loggerName :: String
loggerName = "Thentos"

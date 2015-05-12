module Thentos.Frontend.Util where

import Control.Exception (SomeException)
import Control.Monad (when)
import Control.Monad.CatchIO (try)
import Control.Monad.IO.Class (liftIO)
import Snap (Snap, Config, getOther, SnapletInit, runSnaplet, combineConfig, getVerbose)
import Snap.Http.Server (simpleHttpServe)
import Snap.Snaplet.Config (AppConfig(appEnvironment))
import System.Directory (createDirectoryIfMissing)
import System.IO (stderr, hPutStrLn)

import qualified Data.Text as ST

-- * missing in snap

-- | This does the same as serveSnaplet from the snap package, except that
-- it does not try to read app arguments from the command line
--
-- FIXME: there was some discussion over at snap on how to move that
-- upstream.  what's the state on that?  i think we will have to wait
-- for release 1.0?
serveSnaplet :: Config Snap AppConfig
                 -- ^ The configuration of the server - you can usually pass a
                 -- default 'Config' via
                 -- 'Snap.Http.Server.Config.defaultConfig'.
             -> SnapletInit b b
                 -- ^ The snaplet initializer function.
             -> IO ()
serveSnaplet config initializer = do
    let env = appEnvironment =<< getOther config
    (msgs, handler, doCleanup) <- runSnaplet env initializer

    (conf, site) <- combineConfig config handler
    createDirectoryIfMissing False "log"
    let serve = simpleHttpServe conf

    when (loggingEnabled conf) . liftIO . hPutStrLn stderr $ ST.unpack msgs
    _ <- try (serve site)
         :: IO (Either SomeException ())
    doCleanup
  where
    loggingEnabled = not . (== Just False) . getVerbose

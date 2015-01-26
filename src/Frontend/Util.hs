module Frontend.Util (serveSnaplet) where

import Snap (Snap, Config, getOther, SnapletInit, runSnaplet, when, liftIO, combineConfig, getVerbose)
import Snap.Http.Server (simpleHttpServe)
import Snap.Snaplet.Config (AppConfig(appEnvironment))
import Control.Monad.CatchIO (try)
import qualified Data.Text as T
import System.IO (stderr, hPutStrLn)
import System.Directory (createDirectoryIfMissing)
import Control.Exception (SomeException)

-- | This does the same as serveSnaplet from the snap package, except that
-- it does not try to read app arguments from the command line
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

    when (loggingEnabled conf) . liftIO . hPutStrLn stderr $ T.unpack msgs
    _ <- try (serve site)
         :: IO (Either SomeException ())
    doCleanup
  where
    loggingEnabled = not . (== Just False) . getVerbose

{-# LANGUAGE OverloadedStrings  #-}

module Snap.Missing where

import Control.Exception (SomeException)
import Control.Monad.CatchIO (try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Data.String.Conversions (cs)
import Snap.Core (MonadSnap (..), addHeader, modifyResponse, writeText)
import Snap.Http.Server (simpleHttpServe)
import Snap (Snap, Config, getOther, SnapletInit, runSnaplet, combineConfig, getVerbose)
import Snap.Snaplet.Config (AppConfig(appEnvironment))
import System.Directory (createDirectoryIfMissing)
import System.IO (stderr, hPutStrLn)
import Text.Blaze.Html (Html)

import qualified Data.Text as ST
import qualified Text.Blaze.Html.Renderer.Pretty (renderHtml)


-- | This does the same as serveSnaplet from the snap package, except that
-- it does not try to read app arguments from the command line.
--
-- (Missing in "Snap.Snaplet"; package snap.)
--
-- See also: https://github.com/snapframework/snap/pull/135
-- (Merged on 2015-02-02, but not released as of 2015-05-22.)
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


-- | Write some 'Html' as response.  We use pretty printing instead of
-- 'Text.Blaze.Html.Renderer.Utf8.renderHtml'.  This may even be a good idea in production, because
-- it makes things more transparent while it is not clear how much performance improvement can come
-- from killing all the redundant whitespace.  (If we only didn't have to go through 'String'..)
--
-- (Missing in "Snap.Blaze"; package snap-blaze.)
--
-- No pull request has been made for this.  snap-blaze contains nothing beyond this function, and we
-- can afford inlining it.
blaze :: MonadSnap m => Html -> m ()
blaze response = do
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    writeText . cs $ Text.Blaze.Html.Renderer.Pretty.renderHtml response

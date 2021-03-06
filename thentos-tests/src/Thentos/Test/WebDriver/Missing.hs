{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Thentos.Test.WebDriver.Missing where

import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (catches, Handler(Handler), throwIO)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.String.Conversions (ST)

import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Class as WD
import qualified Test.WebDriver.Commands.Wait as WD


-- | Use this instead of 'WD.openPage' in order to avoid race conditions.  Uses 'waitForPageLoad'.
openPageSync :: (MonadIO wd, WD.WebDriver wd) => String -> wd ()
openPageSync = waitForPageLoad . WD.openPage

-- | Like 'openPageSync' for 'WD.click'.
clickSync :: (MonadIO wd, WD.WebDriver wd) => WD.Element -> wd ()
clickSync = waitForPageLoad . WD.click

-- | If you have an action @act@ that you know will load a new page, and you want this page to be
-- loaded and ready before the action returns, call @waitForPageLoad act@ instead.  See also:
--
--   * http://www.obeythetestinggoat.com/how-to-get-selenium-to-wait-for-page-load-after-a-click.html
--
--   * https://github.com/kallisti-dev/hs-webdriver/issues/72
--
-- FIXME: There are horrible things going on in here like seemingly arbitrary calls to threadDelay,
-- and timeout values that may be exceeded by a lot in practice.  Using webdriver is not for the
-- impatient.
waitForPageLoad :: forall wd a . (MonadIO wd, WD.WebDriver wd) => wd a -> wd a
waitForPageLoad action = do
    let freq    :: Int    = 92000  -- (microseconds)
        timeout :: Double = 13     -- (seconds)
        findHtml :: wd WD.Element = WD.findElem . WD.ByTag $ "html"

    -- first get old html tag, then call action
    html <- findHtml
    result <- action

    -- wait until the old html tag is stale
    waitForElementToGoStale freq timeout html

    -- wait until a new html tag shows up
    _ <- WD.waitUntil' freq timeout findHtml

    -- voodoo, yes.  but perhaps it'll help?
    liftIO . threadDelay $ 38 * 1000

    -- return result produced by action earlier
    return result

-- | We can't use 'waitWhile'' for this because that does not catch @FailedCommand
-- StaleElementReference _@.
waitForElementToGoStale :: forall wd . (MonadIO wd, WD.WebDriver wd) => Int -> Double -> WD.Element -> wd ()
waitForElementToGoStale freq timeout el = loop timeout
  where
    loop :: Double -> wd ()
    loop timeLeft = if timeLeft < 0
        then liftIO . throwIO $ WD.FailedCommand WD.Timeout
              (WD.FailedCommandInfo "`waitForElementToGoStale` has timed out." Nothing Nothing Nothing [])
        else do
            liftIO $ threadDelay freq
            eResult :: Either () ST <- (Right <$> WD.getText el) `catches` [Handler ackStaleException]
            either (\ (_ :: ()) -> return ())
                   (\ (_ :: ST) -> loop $ timeLeft - (fromIntegral freq / 1e6))
                eResult

-- | Return @Left ()@ iff exception is 'StaleElementReference'.  Re-throw all other exceptions.
ackStaleException :: forall wd . (MonadIO wd, MonadBase IO wd, WD.WebDriver wd) => WD.FailedCommand -> wd (Either () ST)
ackStaleException (WD.FailedCommand WD.StaleElementReference _) = return $ Left ()
ackStaleException e                                             = throwIO e

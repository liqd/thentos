{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}

module Thentos.FrontendSpec where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Exception (assert)
import Control.Monad.IO.Class (liftIO)
import Data.SafeCopy (safeGet, safePut)
import Data.Serialize.Get (runGet)
import Data.Serialize.Put (runPut)
import Data.String.Conversions (ST)
import LIO (canFlowTo, lub, glb)
import LIO.DCLabel ((%%), (/\), (\/), toCNF)
import Network.Wai.Test (srequest, simpleStatus, simpleBody, runSession)
import Test.Hspec.QuickCheck (modifyMaxSize)
import Test.Hspec (Spec, describe, it, before, after, shouldBe, shouldThrow, anyException, hspec)
import Test.Hspec (Spec, describe, it, shouldBe, hspec)
import Test.QuickCheck (property)

import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Class as WD
import qualified Test.WebDriver.Config as WD
import qualified Test.WebDriver.Session as WD

import Thentos.Types

import Test.Arbitrary ()
import Test.Util


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = do
    describe "Thentos.Frontend (requires selenium grid)" . before setupTestServerFull . after teardownTestServerFull $ do
        describe "reset password" $
            it "works" $
                   \ ((_, _, _, mkUrl, wd) :: TestServerFull) -> do
                wd $ do
                    WD.openPage (mkUrl "")

                    WD.findElem (WD.ByLinkText "create_user") >>= WD.click

                    let fill :: WD.WebDriver wd => ST -> ST -> wd ()
                        fill label text = WD.findElem (WD.ById label) >>= WD.sendKeys text

                    fill "create_user.name" "username"
                    fill "create_user.password" "password"
                    fill "create_user.email" "email@ad.dress"

                    WD.findElem (WD.ByName "submit") >>= WD.click

                    text <- WD.getSource

                    -- we are thinking about getting hspec-webdriver
                    -- to work here, but there are issues.  meanwhile,
                    -- we can't use hspec infrastructure from within
                    -- @wd@, but we can still crash if something goes
                    -- wrong.
                    assert (text == "Please check your email") $ return ()

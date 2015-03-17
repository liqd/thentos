{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}

module Thentos.FrontendSpec where

import Control.Exception (assert)
import Control.Lens
import Data.Acid.Advanced
import Data.Map (size)
import Data.String.Conversions (ST, cs)
import Test.Hspec (Spec, describe, it, before, after, shouldBe, hspec)
import Text.Regex.Easy ((=~#))

import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Class as WD

import Thentos.DB.Protect
import Thentos.DB.Trans
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
                   \ (((st, _, _), _, _, mkUrl, wd) :: TestServerFull) -> do

                -- create confirmation token
                wd $ do
                    WD.setImplicitWait 1000
                    WD.setScriptTimeout 1000
                    WD.setPageLoadTimeout 1000

                    WD.openPage (mkUrl "")

                    WD.findElem (WD.ByLinkText "create_user") >>= WD.click

                    let fill :: WD.WebDriver wd => ST -> ST -> wd ()
                        fill label text = WD.findElem (WD.ById label) >>= WD.sendKeys text

                    fill "create_user.name" "username"
                    fill "create_user.password" "password"
                    fill "create_user.email" "email@ad.dress"

                    WD.findElem (WD.ById "create_user_submit") >>= WD.click
                    text <- WD.getSource

                    -- we are thinking about getting hspec-webdriver
                    -- to work here, but there are issues.  meanwhile,
                    -- we can't use hspec infrastructure from within
                    -- @wd@, but we can still crash if something goes
                    -- wrong.
                    assert (cs text =~# "Please check your email") $ return ()

                -- check that confirmation token is in DB.
                Right (db :: DB) <- query' st $ SnapShot allowEverything
                size (db ^. dbUnconfirmedUsers) `shouldBe` 1

                -- click activation link.  (FUTURE WORK: it would be
                -- nice if we somehow had the email here to extract
                -- the link from there.)
                -- ...

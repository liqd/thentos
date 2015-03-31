{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Thentos.FrontendSpec where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Acid.Advanced (query')
import Data.Either (isRight)
import Data.String.Conversions (ST, cs)
import Test.Hspec (Spec, SpecWith, describe, it, before, after, shouldBe, shouldSatisfy, hspec, pendingWith)
import Text.Regex.Easy ((=~#), (=~-))

import qualified Data.Map as Map
import qualified Network.HTTP.Types.Status as C
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Class as WD

import Thentos.Config
import Thentos.DB.Protect
import Thentos.DB.Trans
import Thentos.Frontend (urlSignupConfirm)
import Thentos.Types
import Thentos.Util ((<//>))

import Test.Arbitrary ()
import Test.Util


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "selenium (consult README.md if this test fails)"
           . before setupTestServerFull . after teardownTestServerFull $ do
    createUser
    resetPassword
    updateSelf
    logIntoThentos
    logOutOfThentos
    serviceCreate
    serviceDelete
    serviceUpdateMetadata
    serviceGiveToOtherUser
    logIntoService
    logOutOfService
    browseMyServices


createUser :: SpecWith TestServerFull
createUser = it "create user" $ \ (((st, _, _), _, (_, feConfig), wd) :: TestServerFull) -> do
    let myUsername = "username"
        myPassword = "password"
        myEmail    = "email@example.com"

    -- create confirmation token
    wd $ do
        WD.openPage (cs $ exposeUrl feConfig)
        WD.findElem (WD.ByLinkText "create_user") >>= WD.click

        let fill :: WD.WebDriver wd => ST -> ST -> wd ()
            fill label text = WD.findElem (WD.ById label) >>= WD.sendKeys text

        fill "create_user.name" myUsername
        fill "create_user.password1" myPassword
        fill "create_user.password2" myPassword
        fill "create_user.email" myEmail

        WD.findElem (WD.ById "create_user_submit") >>= WD.click
        WD.getSource >>= \ s -> liftIO $ (cs s) `shouldSatisfy` (=~# "Please check your email")

    -- check that confirmation token is in DB.
    Right (db1 :: DB) <- query' st $ SnapShot allowEverything
    Map.size (db1 ^. dbUnconfirmedUsers) `shouldBe` 1

    -- click activation link.  (it would be nice if we
    -- somehow had the email here to extract the link from
    -- there, but we don't.)
    case Map.toList $ db1 ^. dbUnconfirmedUsers of
          [(tok, _)] -> wd $ do
              WD.openPage . cs $ urlSignupConfirm feConfig tok
              WD.getSource >>= \ s -> liftIO $ cs s `shouldSatisfy` (=~# "Added a user!")
          bad -> error $ "dbUnconfirmedUsers: " ++ show bad

    -- check that user has arrived in DB.
    Right (db2 :: DB) <- query' st $ SnapShot allowEverything
    Map.size (db2 ^. dbUnconfirmedUsers) `shouldBe` 0
    eUser <- query' st $ LookupUserByName (UserName myUsername) allowEverything
    eUser `shouldSatisfy` isRight


resetPassword :: SpecWith TestServerFull
resetPassword = it "reset password" $ \ (_ :: TestServerFull) -> pendingWith "no test implemented."


updateSelf :: SpecWith TestServerFull
updateSelf = it "update self" $ \ (_ :: TestServerFull) -> pendingWith "no test implemented."


logIntoThentos :: SpecWith TestServerFull
logIntoThentos = it "log into thentos" $ \ ((_, _, (_, feConfig), wd) :: TestServerFull) -> wd $ do
    wdLogin feConfig "god" "god" >>= liftIO . (`shouldBe` 200) . C.statusCode
    WD.getSource >>= \ s -> liftIO $ (cs s) `shouldSatisfy` (=~# "You are logged in.")
        -- FIXME: bad regexp.  just anything that is not trivially true will suffice.

    -- (out of curiousity: why do we need the type signature in the
    -- lambda parameter?  shouldn't ghc infer (and be happy with the
    -- fact) that the lambda is polymorphic in all places where it
    -- takes '_'?)


logOutOfThentos :: SpecWith TestServerFull
logOutOfThentos = it "log out of thentos" $ \ ((_, _, (_, feConfig), wd) :: TestServerFull) -> wd $ do
    wdLogout feConfig >>= liftIO . (`shouldBe` 200) . C.statusCode
    WD.getSource >>= \ s -> liftIO $ (cs s) `shouldSatisfy` (=~# "You are logged out.")
        -- FIXME: bad regexp.  just anything that is not trivially true will suffice.


serviceCreate :: SpecWith TestServerFull
serviceCreate = it "service create" $ \ (((st, _, _), _, (_, feConfig), wd) :: TestServerFull) -> do
    -- fe: fill out and submit create-service form
    serviceId :: ServiceId <- wd $ do
        wdLogin feConfig "god" "god" >>= liftIO . (`shouldBe` 200) . C.statusCode
        WD.openPage (cs $ exposeUrl feConfig <//> "/service_create")

        let fill :: WD.WebDriver wd => ST -> ST -> wd ()
            fill label text = WD.findElem (WD.ById label) >>= WD.sendKeys text

            sname :: ST = "Evil Corp."
            sdescr :: ST = "don't be evil."

        fill "service_name" sname
        fill "service_description" sdescr

        WD.findElem (WD.ById "service_create_submit") >>= WD.click
        (\ s -> case cs s =~- "Service id: (.+)" of [_, sid] -> ServiceId $ cs sid) <$> WD.getSource

    -- db: check that
    --   1. service has been created;
    --   2. has right sname, sdescr;
    --   3. has correct owner.
    Right (db :: DB) <- query' st $ SnapShot allowEverything
    case Map.lookup serviceId (db ^. dbServices) of
        Nothing -> error "serviceId not found in db."
        Just service -> do
            service ^. serviceSession     `shouldBe` Nothing
            -- service ^. serviceName        `shouldBe` sname
            -- service ^. serviceDescription `shouldBe` sdescr
            -- service ^. serviceOwner       `shouldBe` UserId 0

    -- FIXME: test: without login, create user fails with "permission denied"
    -- FIXME: test: if user is deleted, so are all their services.


serviceDelete :: SpecWith TestServerFull
serviceDelete = it "service delete" $ \ (_ :: TestServerFull) -> pendingWith "no test implemented."


serviceUpdateMetadata :: SpecWith TestServerFull
serviceUpdateMetadata = it "service delete" $ \ (_ :: TestServerFull) -> pendingWith "no test implemented."


serviceGiveToOtherUser :: SpecWith TestServerFull
serviceGiveToOtherUser = it "service delete" $ \ (_ :: TestServerFull) -> pendingWith "no test implemented."


logIntoService :: SpecWith TestServerFull
logIntoService = it "log into service" $ \ (_ :: TestServerFull) -> pendingWith "no test implemented."


logOutOfService :: SpecWith TestServerFull
logOutOfService = it "log out of service" $ \ (_ :: TestServerFull) -> pendingWith "no test implemented."


browseMyServices :: SpecWith TestServerFull
browseMyServices = it "browse my services" $ \ (_ :: TestServerFull) -> pendingWith "no test implemented."
{-
      \ ((st, _, _), _, (_, feConfig), wd) -> do
    wd $ do
        wdLogin "god" "god" >>= liftIO . (`shouldBe` 200)

        -- FIXME:
        -- go to "/user/dashboard"
        -- check that there are services that i'm logged into
        -- check that there are services that i'm logged outof
-}


-- * wd actions

wdLogin :: HttpConfig -> UserName -> UserPass -> WD.WD C.Status
wdLogin feConfig (UserName uname) (UserPass upass) = do
    WD.openPage (cs $ exposeUrl feConfig <//> "log_into_thentos")

    let fill :: WD.WebDriver wd => ST -> ST -> wd ()
        fill label text = WD.findElem (WD.ById label) >>= WD.sendKeys text
    fill "name" uname
    fill "password" upass

    WD.findElem (WD.ById "login_submit") >>= WD.click
    return $ C.Status 200 "Ok."  -- FIXME: we need a man in the middle
                                 -- between browser and http server
                                 -- that we can ask for things
                                 -- happening between the two.
                                 -- selenium doesn't allow that.

wdLogout :: HttpConfig -> WD.WD C.Status
wdLogout feConfig = do
    WD.openPage (cs $ exposeUrl feConfig <//> "logout_thentos")
    return $ C.Status 200 "Ok."  -- FIXME: as in wdLogin

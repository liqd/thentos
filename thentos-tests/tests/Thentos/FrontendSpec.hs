{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Thentos.FrontendSpec where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Data.String.Conversions (ST, cs)
import Test.Hspec (Spec, SpecWith, hspec, around, describe, it,
                   shouldBe, shouldContain, shouldSatisfy, pendingWith)

import qualified Data.Text as ST
import qualified Network.HTTP.Types.Status as C
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Class as WD

import Thentos.Action.Types
import Thentos.Config hiding (getDefaultUser)
import Thentos.Types
import Thentos.Util (verifyUserPass)

import qualified Thentos.Transaction as T

import Thentos.Test.WebDriver.Missing as WD
import Thentos.Test.Arbitrary ()
import Thentos.Test.Config
import Thentos.Test.Core
import Thentos.Test.Transaction


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "selenium grid" . around withFrontendAndBackend $ do
    spec_createUser
    spec_resetPassword
    spec_logIntoThentos
    spec_logOutOfThentos
    spec_redirectWhenNotLoggedIn
    spec_dontRedirectWhenLoggedIn
    spec_deletingCookiesLogsOut
    spec_restoringCookieRestoresSession
    spec_logInSetsSessionCookie
    spec_updateSelf
    spec_serviceCreate
    spec_serviceDelete
    spec_serviceUpdateMetadata
    spec_serviceGiveToOtherUser
    spec_logIntoService
    spec_logOutOfService
    spec_browseMyServices
    spec_failOnCsrf


spec_createUser :: SpecWith ActionEnv
spec_createUser = describe "create user" $ do
    let myUsername = "username"
        myPassword = "password"
        myEmail    = "email@example.com"

    it "fill out form." $ \(ActionEnv cfg connPool) -> do
        withWebDriver $ do
            WD.openPageSync (cs $ exposeUrl (getFrontendConfig cfg))
            WD.findElem (WD.ById "login_create_new") >>= WD.clickSync

            fill "/user/register.name" myUsername
            fill "/user/register.password1" myPassword
            fill "/user/register.password2" myPassword
            fill "/user/register.email" myEmail

            WD.findElem (WD.ById "create_user_submit") >>= WD.clickSync
            liftIO $ pendingWith "this test fails with 'ill-formatted csrf token'"
            WD.getSource >>= \s -> liftIO $ s `shouldSatisfy` ST.isInfixOf "Please check your email"

        -- check that user is in db
        Right (_, usr) <- runVoidedQuery connPool . T.lookupAnyUserByEmail $ forceUserEmail myEmail
        fromUserName  (usr ^. userName)  `shouldBe` myUsername
        fromUserEmail (usr ^. userEmail) `shouldBe` myEmail


spec_resetPassword :: SpecWith ActionEnv
spec_resetPassword = it "reset password" $ \_ -> pendingWith "no test implemented."


spec_updateSelf :: SpecWith ActionEnv
spec_updateSelf = describe "update self" $ do
    let fill_ :: ST -> ST -> WD.WD ()
        fill_ label text = WD.findElem (WD.ById label) >>= (\e -> WD.clearInput e >> WD.sendKeys text e)

        click_ :: ST -> WD.WD ()
        click_ label = WD.findElem (WD.ById label) >>= WD.clickSync

    it "password" $ \(ActionEnv cfg conn) -> do
        (selfId, selfPass, selfName) <- getDefaultUser cfg conn
            -- FIXME: test with ordinary user (not god).
        let newSelfPass = UserPass "da39a3ee5e6b4b0d3255bfef95601890afd80709"
        withWebDriver $ do
            wdLogin (getFrontendConfig cfg) selfName selfPass >>= liftIO . (`shouldBe` 200) . C.statusCode
            WD.openPageSync (cs $ exposeUrl (getFrontendConfig cfg) <//> "/user/update_password")
            fill_ "/user/update_password.old_password"  $ fromUserPass selfPass
            fill_ "/user/update_password.new_password1" $ fromUserPass newSelfPass
            fill_ "/user/update_password.new_password2" $ fromUserPass newSelfPass
            click_ "update_password_submit"
        Right (_, usr) <- runVoidedQuery conn $ T.lookupAnyUser selfId
        usr `shouldSatisfy` verifyUserPass newSelfPass

    -- FIXME: test failure cases.  same restrictions apply as in
    -- "create_user" and "reset_password" (make sure the check is in
    -- separate function, not inlined.)

    it "email" $ \_ ->
        pendingWith "no test implemented."

        {-

        needs another confirmation email.  what to do while waiting for
        re-confirmation?  options:

        1. the user has status "confirmed", but old email is still valid.
           changing email addresses several times without confirmation must work
           as expected: a new attempt overrides the previous ones, and
           invalidates the previous confirmation links.

           good: if the adversary is the user, this is no loss of security (the
           user can just choose not to change the email to the same effect).

           bad: if the adversary is somebody who owns the old address and somehow
           inhibits the confirmation, a potentially invalid/stolen email address
           will remain valid.


        2. the user falls back to status "unconfirmed".  all her existing data
           will remain intact, but she will not be able to access it with her own
           privileges until the confirmation has succeeded.

           good: suggested by frau zabel (datenschutzreferat).

           bad: potentially annoying to the user.


        3. the user has status "confirmed", but email address is replaced by
           an "unconfirmed"-marker.  outgoing emails are queued.

           good: as convenient as 1., (almost) as secure as 2.

           bad: more implementation effort.

        we pick (2) for now, and leave this discussion in the code for
        reference.

        -}


-- manageGroups :: SpecWith ActionEnv
-- manageGroups = describe "manage groups" $ do ...


spec_logIntoThentos :: SpecWith ActionEnv
spec_logIntoThentos = describe "log into thentos" $ do
    describe "with good credentials" $ do
        it "gets you to dashboard" $ \(ActionEnv cfg _) -> withWebDriver $ do
            let (godPass, godName) = getDefaultUser' cfg
            wdLogin (getFrontendConfig cfg) godName godPass >>= liftIO . (`shouldBe` 200) . C.statusCode
            WD.getSource >>= liftIO . (`shouldContain` ("Login successful" :: String)) . cs
    describe "with bad credentials" $ do
        it "gets you back to the login page with a message" $ \(ActionEnv cfg _) -> withWebDriver $ do
            wdLogin (getFrontendConfig cfg) "9187" "916" >>= liftIO . (`shouldBe` 200) . C.statusCode
            WD.getSource >>= liftIO . (`shouldContain` ("Bad username or password" :: String)) . cs


spec_logOutOfThentos :: SpecWith ActionEnv
spec_logOutOfThentos = it "log out of thentos" $ \(ActionEnv cfg _) -> withWebDriver $ do
    let (godPass, godName) = getDefaultUser' cfg

    -- logout when logged in
    wdLogin (getFrontendConfig cfg) godName godPass >>= liftIO . (`shouldBe` 200) . C.statusCode
    wdLogout (getFrontendConfig cfg) >>= liftIO . (`shouldBe` 200) . C.statusCode
    WD.getSource >>= liftIO . (`shouldContain` ("You have been logged out" :: String)) . cs

    -- logout when already logged out
    wdLogout (getFrontendConfig cfg) >>= liftIO . (`shouldBe` 400) . C.statusCode


spec_redirectWhenNotLoggedIn :: SpecWith ActionEnv
spec_redirectWhenNotLoggedIn = it "redirect to login page" $ \(ActionEnv cfg _) -> do
    withWebDriver $ isNotLoggedIn (getFrontendConfig cfg)


spec_dontRedirectWhenLoggedIn :: SpecWith ActionEnv
spec_dontRedirectWhenLoggedIn = it "don't redirect to login page" $ \(ActionEnv cfg _) -> do
    pendingWith "FIXME"
    let (godPass, godName) = getDefaultUser' cfg
    withWebDriver $ do
        wdLogin (getFrontendConfig cfg) godName godPass >>= liftIO . (`shouldBe` 200) . C.statusCode
        isLoggedIn (getFrontendConfig cfg)


spec_deletingCookiesLogsOut :: SpecWith ActionEnv
spec_deletingCookiesLogsOut = it "log out by deleting cookies" $ \(ActionEnv cfg _) -> do
    let (godPass, godName) = getDefaultUser' cfg
    withWebDriver $ do
        wdLogin (getFrontendConfig cfg) godName godPass >>= liftIO . (`shouldBe` 200) . C.statusCode
        WD.deleteVisibleCookies
        isNotLoggedIn (getFrontendConfig cfg)


spec_logInSetsSessionCookie :: SpecWith ActionEnv
spec_logInSetsSessionCookie = it "set cookie on login" $ \(ActionEnv cfg _) -> do
    pendingWith "FIXME"
    let (godPass, godName) = getDefaultUser' cfg
    withWebDriver $ do
        WD.cookies >>= \cc -> liftIO $ cc `shouldBe` []
        wdLogin (getFrontendConfig cfg) godName godPass >>= liftIO . (`shouldBe` 200) . C.statusCode
        WD.cookies >>= \cc -> liftIO $ cc `shouldSatisfy` oneSessionCookie
  where
    oneSessionCookie [c] = WD.cookName c == "sess" && WD.cookPath c == Just "/"
    oneSessionCookie _   = False


-- This is a a webdriver meta-test, as a base case for 'spec_failOnCsrf'.
spec_restoringCookieRestoresSession :: SpecWith ActionEnv
spec_restoringCookieRestoresSession =
    it "restore session by restoring cookie" $ \(ActionEnv cfg _) -> do
        liftIO $ pendingWith "FIXME"
        let (godPass, godName) = getDefaultUser' cfg
        withWebDriver $ do
            wdLogin (getFrontendConfig cfg) godName godPass >>= liftIO . (`shouldBe` 200) . C.statusCode
            cookies <- WD.cookies
            WD.deleteVisibleCookies
            mapM_ WD.setCookie cookies
            isLoggedIn (getFrontendConfig cfg)

        -- With phantomjs, the store/delete/set cycle adds a leading dot to the cookie
        -- domain, and the dashboard request inside 'isLoggedin' above sets
        -- the updated cookie, so we end up with two cookies.
        -- WD.cookies >>= \cc -> liftIO $ length cc `shouldBe` 1


spec_serviceCreate :: SpecWith ActionEnv
spec_serviceCreate = it "service create" $ \(ActionEnv cfg conn) -> do
    pendingWith "FIXME"
    -- fe: fill out and submit create-service form
    let sname :: ST = "Evil Corp."
        sdescr :: ST = "don't be evil."
        pat = "Service id: "
        extractId s = case snd . ST.breakOn "Service id: " $ s of
            "" -> Nothing
            m  -> Just . ServiceId . ST.take 24 . ST.drop (ST.length pat) $ m
        (godPass, godName) = getDefaultUser' cfg
    sid <- withWebDriver $ do
        wdLogin (getFrontendConfig cfg) godName godPass >>= liftIO . (`shouldBe` 200) . C.statusCode
        WD.openPageSync (cs $ exposeUrl (getFrontendConfig cfg) <//> "/service/create")

        fill "/service/create.name" sname
        fill "/service/create.description" sdescr

        WD.findElem (WD.ById "create_service_submit") >>= WD.clickSync
        extractId <$> WD.getSource >>= \sid -> do
            liftIO $ sid `shouldSatisfy` isJust
            return (fromJust sid)

    -- db: check that
    --   1. service has been created;
    --   2. has right sname, sdescr;
    --   3. has correct owner.
    Right (_, service) <- runVoidedQuery conn $ T.lookupService sid
    service ^. serviceThentosSession `shouldBe` Nothing
    service ^. serviceName           `shouldBe` ServiceName sname
    service ^. serviceDescription    `shouldBe` ServiceDescription sdescr
    -- service ^. serviceOwner          `shouldBe` UserId 0

    -- FIXME: test: without login, create user fails with "permission denied"
    -- FIXME: test: if user is deleted, so are all their services.


spec_serviceDelete :: SpecWith ActionEnv
spec_serviceDelete = it "service delete" $ \(_ :: ActionEnv) -> pendingWith "no test implemented."


spec_serviceUpdateMetadata :: SpecWith ActionEnv
spec_serviceUpdateMetadata = it "service delete" $ \(_ :: ActionEnv) -> pendingWith "no test implemented."


spec_serviceGiveToOtherUser :: SpecWith ActionEnv
spec_serviceGiveToOtherUser = it "service delete" $ \(_ :: ActionEnv) -> pendingWith "no test implemented."


spec_logIntoService :: SpecWith ActionEnv
spec_logIntoService = it "log into service" $ \(_ :: ActionEnv) -> pendingWith "no test implemented."


spec_logOutOfService :: SpecWith ActionEnv
spec_logOutOfService = it "log out of service" $ \(_ :: ActionEnv) -> pendingWith "no test implemented."


spec_browseMyServices :: SpecWith ActionEnv
spec_browseMyServices = it "browse my services" $ \(_ :: ActionEnv) -> pendingWith "no test implemented."


spec_failOnCsrf :: SpecWith ActionEnv
spec_failOnCsrf =  it "fails on csrf" $ \(ActionEnv cfg _) -> withWebDriver $ do
    liftIO $ pendingWith "FIXME"

    let (godPass, godName) = getDefaultUser' cfg
    wdLogin (getFrontendConfig cfg) godName godPass >>= liftIO . (`shouldBe` 200) . C.statusCode
    storedCookies <- WD.cookies
    WD.deleteVisibleCookies
    wdLogin (getFrontendConfig cfg) godName godPass >>= liftIO . (`shouldBe` 200) . C.statusCode
    WD.openPageSync (cs $ exposeUrl (getFrontendConfig cfg) <//> "/service/create")
    WD.deleteVisibleCookies -- delete before restore to work around phantomjs domain wonkiness
    mapM_ WD.setCookie storedCookies
    fill "/service/create.name" "this is a service name"
    fill "/service/create.description" "this is a service description"
    WD.findElem (WD.ById "create_service_submit") >>= WD.clickSync
    WD.getSource >>= \s -> liftIO $ s `shouldSatisfy` ST.isInfixOf "csrf badness"


-- * wd actions

wdLogin :: HttpConfig -> UserName -> UserPass -> WD.WD C.Status
wdLogin feConfig (UserName uname) (UserPass upass) = do
    WD.setImplicitWait 200
    WD.openPageSync (cs $ exposeUrl feConfig <//> "/user/login")

    fill "/user/login.name" uname
    fill "/user/login.password" upass

    WD.findElem (WD.ById "login_submit") >>= WD.clickSync
    return $ C.Status 200 "Ok."  -- (we need a man in the middle
                                 -- between browser and http server
                                 -- that we can ask for things
                                 -- happening between the two.
                                 -- selenium doesn't allow that.)

wdLogout :: HttpConfig -> WD.WD C.Status
wdLogout feConfig = do
    WD.openPageSync (cs $ exposeUrl feConfig <//> "/user/logout")
    WD.findElems (WD.ById "logout_submit") >>= maybe noButton buttonIsThere . listToMaybe
  where
    noButton = do
        return $ C.Status 400 "Perhaps we are already logged out?"
    buttonIsThere el = do
        WD.clickSync el
        return $ C.Status 200 "Ok."  -- (see comment in wdLogin.)


isLoggedIn :: HttpConfig -> WD.WD ()
isLoggedIn cfg = do
        WD.openPageSync (cs $ exposeUrl cfg <//> "/dashboard/details")
        WD.getSource >>= \s -> liftIO $ s `shouldSatisfy` ST.isInfixOf "Thentos Dashboard"

isNotLoggedIn :: HttpConfig -> WD.WD ()
isNotLoggedIn cfg = do
        WD.openPageSync (cs $ exposeUrl cfg <//> "/dashboard/details")
        WD.getSource >>= \s -> liftIO $ s `shouldSatisfy` ST.isInfixOf "Thentos Login"


-- | Fill a labeled text field.
fill :: WD.WebDriver wd => ST -> ST -> wd ()
fill label text = WD.findElem (WD.ById label) >>= WD.sendKeys text

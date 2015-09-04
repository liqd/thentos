{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Thentos.FrontendSpec where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Data.String.Conversions (ST, cs)
import Data.Void (Void)
import Test.Hspec (Spec, SpecWith, around, describe, it, shouldBe, shouldSatisfy, hspec, pendingWith)

import qualified Data.Text as ST
import qualified Network.HTTP.Types.Status as C
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Class as WD

import Thentos.Action.Core
import Thentos.Config
import qualified Thentos.Transaction.Transactions as T
import Thentos.Transaction.Core (runThentosQuery)
import Thentos.Types
import Thentos.Util ((<//>))

import Thentos.Test.WebDriver.Missing as WD
import Thentos.Test.Arbitrary ()
import Thentos.Test.Config (godName, godPass)
import Thentos.Test.Core


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "selenium grid" $ do
  describe "many tests" . around withFrontendAndBackend $ do
    spec_createUser
    spec_resetPassword
    spec_logIntoThentos
    spec_logOutOfThentos
    spec_redirectWhenNotLoggedIn
    spec_dontRedirectWhenLoggedIn
    spec_deletingCookiesLogsOut
    spec_logInSetsSessionCookie
    spec_restoringCookieRestoresSession
    spec_serviceCreate
    spec_serviceDelete
    spec_serviceUpdateMetadata
    spec_serviceGiveToOtherUser
    spec_logIntoService
    spec_logOutOfService
    spec_browseMyServices
    spec_failOnCsrf

  -- (this is a separate top-level test case because it changes the DB
  -- state and gets the other tests confused.)
  describe "update user" . around withFrontendAndBackend $ do
    spec_updateSelf


spec_createUser :: SpecWith ActionState
spec_createUser = describe "create user" $ do
    let myUsername = "username"
        myPassword = "password"
        myEmail    = "email@example.com"

    it "fill out form." $ \(ActionState (st, _, _)) -> do
        withWebDriver $ do
            WD.openPageSync (cs $ exposeUrl defaultFrontendConfig)
            WD.findElem (WD.ByLinkText "Register new user") >>= WD.clickSync

            fill "/user/register.name" myUsername
            fill "/user/register.password1" myPassword
            fill "/user/register.password2" myPassword
            fill "/user/register.email" myEmail

            WD.findElem (WD.ById "create_user_submit") >>= WD.clickSync
            WD.getSource >>= \s -> liftIO $ s `shouldSatisfy` ST.isInfixOf "Please check your email"

        -- check that user is in db
        (eUser :: Either (ThentosError Void) (UserId, User)) <- runThentosQuery st $ T.lookupUserByName (UserName myUsername)
        fromUserName  . (^. userName)  . snd <$> eUser `shouldBe` Right myUsername
        fromUserEmail . (^. userEmail) . snd <$> eUser `shouldBe` Right myEmail


spec_resetPassword :: SpecWith ActionState
spec_resetPassword = it "reset password" $ \_ -> pendingWith "no test implemented."


spec_updateSelf :: SpecWith ActionState
spec_updateSelf = describe "update self" $ do
    let _fill :: ST -> ST -> WD.WD ()
        _fill label text = WD.findElem (WD.ById label) >>= (\e -> WD.clearInput e >> WD.sendKeys text e)

        _click :: ST -> WD.WD ()
        _click label = WD.findElem (WD.ById label) >>= WD.clickSync

        -- FIXME: test with ordinary user (not god).
        selfName = godName
        selfPass = godPass

    it "username" $ \(ActionState _) -> withWebDriver $ do
        let newSelfName = UserName "da39a3ee5e6b4b0d3255bfef95601890afd80709"
        wdLogin defaultFrontendConfig selfName selfPass >>= liftIO . (`shouldBe` 200) . C.statusCode
        WD.openPageSync (cs $ exposeUrl defaultFrontendConfig <//> "/user/update")
        _fill "/user/update.name" $ fromUserName newSelfName
        _click "update_user_submit"

    -- FIXME: test with new user name that is already in use.
    -- FIXME: test with unauthenticated user.
    -- FIXME: test with other user (user A wants to edit uesr B), with and without RoleAdmin.

    it "password" $ \(ActionState _) -> withWebDriver $ do
        let newSelfPass = UserPass "da39a3ee5e6b4b0d3255bfef95601890afd80709"
        wdLogin defaultFrontendConfig selfName selfPass >>= liftIO . (`shouldBe` 200) . C.statusCode
        WD.openPageSync (cs $ exposeUrl defaultFrontendConfig <//> "/user/update_password")
        _fill "/user/update_password.old_password"  $ fromUserPass selfPass
        _fill "/user/update_password.new_password1" $ fromUserPass newSelfPass
        _fill "/user/update_password.new_password2" $ fromUserPass newSelfPass
        _click "update_password_submit"

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


-- manageRoles :: SpecWith ActionState
-- manageRoles = describe "manage roles" $ do ...


spec_logIntoThentos :: SpecWith ActionState
spec_logIntoThentos = it "log into thentos" $ \_ -> withWebDriver $ do
    wdLogin defaultFrontendConfig godName godPass >>= liftIO . (`shouldBe` 200) . C.statusCode
    WD.getSource >>= \s -> liftIO $ s `shouldSatisfy` ST.isInfixOf "Login successful"


spec_logOutOfThentos :: SpecWith ActionState
spec_logOutOfThentos = it "log out of thentos" $ \_ -> withWebDriver $ do
    -- logout when logged in
    wdLogin defaultFrontendConfig godName godPass >>= liftIO . (`shouldBe` 200) . C.statusCode
    wdLogout defaultFrontendConfig >>= liftIO . (`shouldBe` 200) . C.statusCode
    WD.getSource >>= \s -> liftIO $ s `shouldSatisfy` ST.isInfixOf "You have been logged out"

    -- logout when already logged out
    wdLogout defaultFrontendConfig >>= liftIO . (`shouldBe` 400) . C.statusCode


spec_redirectWhenNotLoggedIn :: SpecWith ActionState
spec_redirectWhenNotLoggedIn = it "redirect to login page" $ \_ -> do
    withWebDriver $ isNotLoggedIn defaultFrontendConfig


spec_dontRedirectWhenLoggedIn :: SpecWith ActionState
spec_dontRedirectWhenLoggedIn = it "don't redirect to login page" $ \_ -> do
    withWebDriver $ do
        wdLogin defaultFrontendConfig godName godPass >>= liftIO . (`shouldBe` 200) . C.statusCode
        isLoggedIn defaultFrontendConfig


spec_deletingCookiesLogsOut :: SpecWith ActionState
spec_deletingCookiesLogsOut = it "log out by deleting cookies" $ \_ -> do
    withWebDriver $ do
        wdLogin defaultFrontendConfig godName godPass >>= liftIO . (`shouldBe` 200) . C.statusCode
        WD.deleteVisibleCookies
        isNotLoggedIn defaultFrontendConfig


spec_logInSetsSessionCookie :: SpecWith ActionState
spec_logInSetsSessionCookie = it "set cookie on login" $ \_ -> do
    withWebDriver $ do
        WD.cookies >>= \cc -> liftIO $ cc `shouldBe` []
        wdLogin defaultFrontendConfig godName godPass >>= liftIO . (`shouldBe` 200) . C.statusCode
        WD.cookies >>= \cc -> liftIO $ cc `shouldSatisfy` oneSessionCookie
  where
    oneSessionCookie [c] = WD.cookName c == "sess" && WD.cookPath c == Just "/"
    oneSessionCookie _   = False


-- This is a a webdriver meta-test, as a base case for 'spec_failOnCsrf'.
spec_restoringCookieRestoresSession :: SpecWith ActionState
spec_restoringCookieRestoresSession = it "restore session by restoring cookie" $ \_ -> do
    withWebDriver $ do
        wdLogin defaultFrontendConfig godName godPass >>= liftIO . (`shouldBe` 200) . C.statusCode
        cookies <- WD.cookies
        WD.deleteVisibleCookies
        mapM_ WD.setCookie cookies

        isLoggedIn defaultFrontendConfig

        -- With phantomjs, the store/delete/set cycle adds a leading dot to the cookie
        -- domain, and the dashboard request inside 'isLoggedin' above sets
        -- the updated cookie, so we end up with two cookies.
        -- WD.cookies >>= \cc -> liftIO $ length cc `shouldBe` 1


spec_serviceCreate :: SpecWith ActionState
spec_serviceCreate = it "service create" $ \(ActionState _) -> do
    -- fe: fill out and submit create-service form
    let sname :: ST = "Evil Corp."
        sdescr :: ST = "don't be evil."
        pat = "Service id: "
        extractId s = case snd . ST.breakOn "Service id: " $ s of
            "" -> Nothing
            m  -> Just . ServiceId . ST.take 24 . ST.drop (ST.length pat) $ m
    _serviceId <- withWebDriver $ do
        wdLogin defaultFrontendConfig godName godPass >>= liftIO . (`shouldBe` 200) . C.statusCode
        WD.openPageSync (cs $ exposeUrl defaultFrontendConfig <//> "/dashboard/ownservices")

        fill "/dashboard/ownservices.name" sname
        fill "/dashboard/ownservices.description" sdescr

        WD.findElem (WD.ById "create_service_submit") >>= WD.clickSync
        extractId <$> WD.getSource >>= \sid -> do
            liftIO $ sid `shouldSatisfy` isJust
            return (fromJust sid)
    return ()

    -- db: check that
    --   1. service has been created;
    --   2. has right sname, sdescr;
    --   3. has correct owner.
    -- FIXME: look this up via the API
    --Right (db :: DB) <- query' st $ SnapShot
    --case Map.lookup serviceId (db ^. dbServices) of
    --    Nothing -> error "serviceId not found in db."
    --    Just service -> do
    --        service ^. serviceThentosSession `shouldBe` Nothing
    --        service ^. serviceName           `shouldBe` ServiceName sname
    --        service ^. serviceDescription    `shouldBe` ServiceDescription sdescr
    --        -- service ^. serviceOwner          `shouldBe` UserId 0

    -- FIXME: test: without login, create user fails with "permission denied"
    -- FIXME: test: if user is deleted, so are all their services.


spec_serviceDelete :: SpecWith ActionState
spec_serviceDelete = it "service delete" $ \(_ :: ActionState) -> pendingWith "no test implemented."


spec_serviceUpdateMetadata :: SpecWith ActionState
spec_serviceUpdateMetadata = it "service delete" $ \(_ :: ActionState) -> pendingWith "no test implemented."


spec_serviceGiveToOtherUser :: SpecWith ActionState
spec_serviceGiveToOtherUser = it "service delete" $ \(_ :: ActionState) -> pendingWith "no test implemented."


spec_logIntoService :: SpecWith ActionState
spec_logIntoService = it "log into service" $ \(_ :: ActionState) -> pendingWith "no test implemented."


spec_logOutOfService :: SpecWith ActionState
spec_logOutOfService = it "log out of service" $ \(_ :: ActionState) -> pendingWith "no test implemented."


spec_browseMyServices :: SpecWith ActionState
spec_browseMyServices = it "browse my services" $ \(_ :: ActionState) -> pendingWith "no test implemented."
{-
      \((st, _, _), _, (_, defaultFrontendConfig), wd) -> do
    wd $ do
        wdLogin godName godPass >>= liftIO . (`shouldBe` 200)

        -- FIXME:
        -- go to "/user/dashboard"
        -- check that there are services that i'm logged into
        -- check that there are services that i'm logged outof
-}


spec_failOnCsrf :: SpecWith ActionState
spec_failOnCsrf =  it "fails on csrf" $ \_ -> withWebDriver $ do
    wdLogin defaultFrontendConfig godName godPass >>= liftIO . (`shouldBe` 200) . C.statusCode
    storedCookies <- WD.cookies
    WD.deleteVisibleCookies
    wdLogin defaultFrontendConfig godName godPass >>= liftIO . (`shouldBe` 200) . C.statusCode
    WD.openPageSync (cs $ exposeUrl defaultFrontendConfig <//> "/dashboard/ownservices")
    WD.deleteVisibleCookies -- delete before restore to work around phantomjs domain wonkiness
    mapM_ WD.setCookie storedCookies
    fill "/dashboard/ownservices.name" "this is a service name"
    fill "/dashboard/ownservices.description" "this is a service description"
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
    return $ C.Status 200 "Ok."  -- FIXME: we need a man in the middle
                                 -- between browser and http server
                                 -- that we can ask for things
                                 -- happening between the two.
                                 -- selenium doesn't allow that.

wdLogout :: HttpConfig -> WD.WD C.Status
wdLogout feConfig = do
    WD.openPageSync (cs $ exposeUrl feConfig <//> "/user/logout")
    WD.findElems (WD.ById "logout_submit") >>= maybe noButton buttonIsThere . listToMaybe
  where
    noButton = do
        return $ C.Status 400 "Perhaps we are already logged out?"
    buttonIsThere el = do
        WD.clickSync el
        return $ C.Status 200 "Ok."  -- FIXME: as in wdLogin


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

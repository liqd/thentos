{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Thentos.FrontendSpec where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Acid (AcidState)
import Data.Acid.Advanced (query')
import Data.Maybe (listToMaybe)
import Data.String.Conversions (ST, cs)
import Test.Hspec (Spec, SpecWith, describe, it, before, after, shouldBe, shouldSatisfy, hspec, pendingWith)
import Text.Regex.Easy ((=~#), (=~-))

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types.Status as C
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Class as WD

import Thentos.Action.Core
import Thentos.Config
import Thentos.Frontend.Handlers.Combinators (urlConfirm)
import Thentos.Transaction
import Thentos.Types
import Thentos.Util ((<//>), verifyPass)

import Thentos.Test.WebDriver.Missing as WD
import Thentos.Test.Arbitrary ()
import Thentos.Test.Core
import Thentos.Test.Types


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "selenium grid" $ do
  describe "many tests" . before setupTestServerFull . after teardownTestServerFull $ do
    spec_createUser
    spec_resetPassword
    spec_logIntoThentos
    spec_logOutOfThentos
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
  describe "update user" . before setupTestServerFull . after teardownTestServerFull $ do
    spec_updateSelf


spec_createUser :: SpecWith FTS
spec_createUser = describe "create user" $ do
    let myUsername = "username"
        myPassword = "password"
        myEmail    = "email@example.com"

    it "fill out form." $ \ fts -> do
        (fts ^. ftsRunWD) $ do
            WD.openPageSync (cs $ exposeUrl (fts ^. ftsFrontendCfg))
            WD.findElem (WD.ByLinkText "Register new user") >>= WD.clickSync

            let fill :: WD.WebDriver wd => ST -> ST -> wd ()
                fill label text = WD.findElem (WD.ById label) >>= WD.sendKeys text

            fill "/user/register.name" myUsername
            fill "/user/register.password1" myPassword
            fill "/user/register.password2" myPassword
            fill "/user/register.email" myEmail

            WD.findElem (WD.ById "create_user_submit") >>= WD.clickSync
            WD.getSource >>= \ s -> liftIO $ cs s `shouldSatisfy` (=~# "Please check your email")

        let ActionState (st, _, _) = fts ^. ftsActionState
            feConfig = fts ^. ftsFrontendCfg
            wd = fts ^. ftsRunWD

        -- check confirmation token in DB.
        Right (db1 :: DB) <- query' st $ SnapShot
        Map.size (db1 ^. dbUnconfirmedUsers) `shouldBe` 1

        -- (it would be nice if we somehow had access to the email here to extract the link from
        -- there.  not yet...)
        case Map.toList $ db1 ^. dbUnconfirmedUsers of
              [(tok, _)] -> wd $ do
                  WD.openPageSync . cs $ urlConfirm feConfig "/user/register_confirm" (fromConfirmationToken tok)
                  WD.getSource >>= \ s -> liftIO $ cs s `shouldSatisfy` (=~# "Registration complete")
              bad -> error $ "dbUnconfirmedUsers: " ++ show bad

        -- check that user is in db
        Right (db2 :: DB) <- query' st $ SnapShot
        Map.size (db2 ^. dbUnconfirmedUsers) `shouldBe` 0
        eUser <- query' st $ LookupUserByName (UserName myUsername)
        fromUserName  . (^. userName)  . snd <$> eUser `shouldBe` Right myUsername
        fromUserEmail . (^. userEmail) . snd <$> eUser `shouldBe` Right myEmail


spec_resetPassword :: SpecWith FTS
spec_resetPassword = it "reset password" $ \ (_ :: FTS) -> pendingWith "no test implemented."


spec_updateSelf :: SpecWith FTS
spec_updateSelf = describe "update self" $ do
    let _fill :: ST -> ST -> WD.WD ()
        _fill label text = WD.findElem (WD.ById label) >>= (\e -> WD.clearInput e >> WD.sendKeys text e)

        _click :: ST -> WD.WD ()
        _click label = WD.findElem (WD.ById label) >>= WD.clickSync

        _check ::  AcidState DB -> (User -> IO ()) -> WD.WD ()
        _check st f = liftIO $ query' st SnapShot >>=
                        maybe (error "no such user") f .
                        either (error "could not take db snapshot") (Map.lookup (UserId selfId) . (^. dbUsers))

        -- FIXME: test with ordinary user (not god).
        selfId   :: Integer = 0
        selfName :: ST      = "god"
        selfPass :: ST      = "god"

    it "username" $ \ fts -> (fts ^. ftsRunWD) $ do
        let ActionState (st, _, _) = fts ^. ftsActionState
            feConfig = fts ^. ftsFrontendCfg

        let newSelfName :: ST = "da39a3ee5e6b4b0d3255bfef95601890afd80709"
        wdLogin feConfig (UserName selfName) (UserPass selfPass) >>= liftIO . (`shouldBe` 200) . C.statusCode
        WD.openPageSync (cs $ exposeUrl feConfig <//> "/user/update")
        _fill "/user/update.name" newSelfName
        _click "update_user_submit"
        _check st ((`shouldBe` UserName newSelfName) . (^. userName))

    -- FIXME: test with new user name that is already in use.
    -- FIXME: test with unauthenticated user.
    -- FIXME: test with other user (user A wants to edit uesr B), with and without RoleAdmin.

    it "password" $ \ fts -> fts ^. ftsRunWD $ do
        let ActionState (st, _, _) = fts ^. ftsActionState
            feConfig = fts ^. ftsFrontendCfg

        let newSelfPass :: ST = "da39a3ee5e6b4b0d3255bfef95601890afd80709"
        wdLogin feConfig (UserName selfName) (UserPass selfPass) >>= liftIO . (`shouldBe` 200) . C.statusCode
        WD.openPageSync (cs $ exposeUrl feConfig <//> "/user/update_password")
        _fill "/user/update_password.old_password" selfPass
        _fill "/user/update_password.new_password1" newSelfPass
        _fill "/user/update_password.new_password2" newSelfPass
        _click "update_password_submit"
        _check st (`shouldSatisfy` verifyPass (UserPass newSelfPass))

    -- FIXME: test failure cases.  same restrictions apply as in
    -- "create_user" and "reset_password" (make sure the check is in
    -- separate function, not inlined.)

    it "email" $ \ (_ :: FTS) ->
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


-- manageRoles :: SpecWith FTS
-- manageRoles = describe "manage roles" $ do ...


spec_logIntoThentos :: SpecWith FTS
spec_logIntoThentos = it "log into thentos" $ \ fts -> fts ^. ftsRunWD $ do
    let feConfig = fts ^. ftsFrontendCfg

    wdLogin feConfig "god" "god" >>= liftIO . (`shouldBe` 200) . C.statusCode
    WD.getSource >>= \ s -> liftIO $ cs s `shouldSatisfy` (=~# "Login successful")

    -- (out of curiousity: why do we need the type signature in the
    -- lambda parameter?  shouldn't ghc infer (and be happy with the
    -- fact) that the lambda is polymorphic in all places where it
    -- takes '_'?)

spec_logOutOfThentos :: SpecWith FTS
spec_logOutOfThentos = it "log out of thentos" $ \ fts -> fts ^. ftsRunWD $ do
    let feConfig = fts ^. ftsFrontendCfg

    -- logout when logged in
    wdLogin feConfig "god" "god" >>= liftIO . (`shouldBe` 200) . C.statusCode
    wdLogout feConfig >>= liftIO . (`shouldBe` 200) . C.statusCode
    WD.getSource >>= \ s -> liftIO $ cs s `shouldSatisfy` (=~# "You have been logged out")

    -- logout when already logged out
    wdLogout feConfig >>= liftIO . (`shouldBe` 400) . C.statusCode


spec_serviceCreate :: SpecWith FTS
spec_serviceCreate = it "service create" $ \ fts -> do
    let ActionState (st, _, _) = fts ^. ftsActionState
        feConfig = fts ^. ftsFrontendCfg
        wd = fts ^. ftsRunWD

    -- fe: fill out and submit create-service form
    let sname :: ST = "Evil Corp."
        sdescr :: ST = "don't be evil."
    serviceId :: ServiceId <- wd $ do
        wdLogin feConfig "god" "god" >>= liftIO . (`shouldBe` 200) . C.statusCode
        WD.openPageSync (cs $ exposeUrl feConfig <//> "/dashboard/ownservices")

        let fill :: WD.WebDriver wd => ST -> ST -> wd ()
            fill label text = WD.findElem (WD.ById label) >>= WD.sendKeys text

        fill "/dashboard/ownservices.name" sname
        fill "/dashboard/ownservices.description" sdescr

        WD.findElem (WD.ById "create_service_submit") >>= WD.clickSync

        (\ ((=~- "Service id: (.+)\"") . cs -> [_, sid]) -> ServiceId $ cs (LBS.take 24 sid)) <$> WD.getSource

    -- db: check that
    --   1. service has been created;
    --   2. has right sname, sdescr;
    --   3. has correct owner.
    Right (db :: DB) <- query' st $ SnapShot
    case Map.lookup serviceId (db ^. dbServices) of
        Nothing -> error "serviceId not found in db."
        Just service -> do
            service ^. serviceThentosSession `shouldBe` Nothing
            service ^. serviceName           `shouldBe` ServiceName sname
            service ^. serviceDescription    `shouldBe` ServiceDescription sdescr
            -- service ^. serviceOwner          `shouldBe` UserId 0

    -- FIXME: test: without login, create user fails with "permission denied"
    -- FIXME: test: if user is deleted, so are all their services.


spec_serviceDelete :: SpecWith FTS
spec_serviceDelete = it "service delete" $ \ (_ :: FTS) -> pendingWith "no test implemented."


spec_serviceUpdateMetadata :: SpecWith FTS
spec_serviceUpdateMetadata = it "service delete" $ \ (_ :: FTS) -> pendingWith "no test implemented."


spec_serviceGiveToOtherUser :: SpecWith FTS
spec_serviceGiveToOtherUser = it "service delete" $ \ (_ :: FTS) -> pendingWith "no test implemented."


spec_logIntoService :: SpecWith FTS
spec_logIntoService = it "log into service" $ \ (_ :: FTS) -> pendingWith "no test implemented."


spec_logOutOfService :: SpecWith FTS
spec_logOutOfService = it "log out of service" $ \ (_ :: FTS) -> pendingWith "no test implemented."


spec_browseMyServices :: SpecWith FTS
spec_browseMyServices = it "browse my services" $ \ (_ :: FTS) -> pendingWith "no test implemented."
{-
      \ ((st, _, _), _, (_, feConfig), wd) -> do
    wd $ do
        wdLogin "god" "god" >>= liftIO . (`shouldBe` 200)

        -- FIXME:
        -- go to "/user/dashboard"
        -- check that there are services that i'm logged into
        -- check that there are services that i'm logged outof
-}


spec_failOnCsrf :: SpecWith FTS
spec_failOnCsrf =  it "fails on csrf" $ \ fts -> fts ^. ftsRunWD $ do
    let feConfig = fts ^. ftsFrontendCfg
    wdLogin feConfig (UserName "god") (UserPass "god") >>= liftIO . (`shouldBe` 200) . C.statusCode
    storedCookies <- WD.cookies
    WD.deleteVisibleCookies
    wdLogin feConfig (UserName "god") (UserPass "god") >>= liftIO . (`shouldBe` 200) . C.statusCode
    WD.openPageSync (cs $ exposeUrl feConfig <//> "/dashboard/ownservices")
    mapM_ WD.setCookie storedCookies
    let fill :: WD.WebDriver wd => ST -> ST -> wd ()
        fill label text = WD.findElem (WD.ById label) >>= WD.sendKeys text

    fill "/dashboard/ownservices.name" "this is a service name"
    fill "/dashboard/ownservices.description" "this is a service description"
    WD.findElem (WD.ById "create_service_submit") >>= WD.clickSync
    WD.getSource >>= \ s -> liftIO $ cs s `shouldSatisfy` (=~# "csrf badness")


-- * wd actions

wdLogin :: HttpConfig -> UserName -> UserPass -> WD.WD C.Status
wdLogin feConfig (UserName uname) (UserPass upass) = do
    WD.setImplicitWait 200
    WD.openPageSync (cs $ exposeUrl feConfig <//> "/user/login")

    let fill :: WD.WebDriver wd => ST -> ST -> wd ()
        fill label text = WD.findElem (WD.ById label) >>= WD.sendKeys text
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

{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Thentos.FrontendSpec where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Acid (AcidState)
import Data.Acid.Advanced (query')
import Data.Either (isRight)
import Data.Maybe (listToMaybe)
import Data.String.Conversions (ST, cs)
import Test.Hspec (Spec, SpecWith, describe, it, before, after, shouldBe, shouldSatisfy, hspec, pendingWith)
import Text.Regex.Easy ((=~#), (=~-))

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as LB
import qualified Network.HTTP.Types.Status as C
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Class as WD

import Thentos.Config
import Thentos.DB.Protect
import Thentos.DB.Trans
import Thentos.Frontend.Handlers.Combinators (urlConfirm)
import Thentos.Types
import Thentos.Util ((<//>), verifyPass)

import Test.Arbitrary ()
import Test.Util


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "selenium (consult README.md if this test fails)" $ do
  describe "many tests" . before setupTestServerFull . after teardownTestServerFull $ do
    createUser
    resetPassword
    logIntoThentos
    logOutOfThentos
    serviceCreate
    serviceDelete
    serviceUpdateMetadata
    serviceGiveToOtherUser
    logIntoService
    logOutOfService
    browseMyServices

  -- (this is a separate top-level test case because it changes the DB
  -- state and gets the other tests confused.)
  describe "update user" . before setupTestServerFull . after teardownTestServerFull $ do
    updateSelf


createUser :: SpecWith TestServerFull
createUser = it "create user" $ \ (((st, _, _), _, (_, feConfig), wd) :: TestServerFull) -> do
    let myUsername = "username"
        myPassword = "password"
        myEmail    = "email@example.com"

    -- create confirmation token
    wd $ do
        WD.openPage (cs $ exposeUrl feConfig)
        WD.findElem (WD.ByLinkText "Register new user") >>= WD.click

        let fill :: WD.WebDriver wd => ST -> ST -> wd ()
            fill label text = WD.findElem (WD.ById label) >>= WD.sendKeys text

        fill "/user/register.name" myUsername
        fill "/user/register.password1" myPassword
        fill "/user/register.password2" myPassword
        fill "/user/register.email" myEmail

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
              WD.openPage . cs $ urlConfirm feConfig "/user/register_confirm" (fromConfirmationToken tok)
              WD.getSource >>= \ s -> liftIO $ cs s `shouldSatisfy` (=~# "Thentos Dashboard")
          bad -> error $ "dbUnconfirmedUsers: " ++ show bad

    -- check that user has arrived in DB.
    Right (db2 :: DB) <- query' st $ SnapShot allowEverything
    Map.size (db2 ^. dbUnconfirmedUsers) `shouldBe` 0
    eUser <- query' st $ LookupUserByName (UserName myUsername) allowEverything
    eUser `shouldSatisfy` isRight


resetPassword :: SpecWith TestServerFull
resetPassword = it "reset password" $ \ (_ :: TestServerFull) -> pendingWith "no test implemented."


updateSelf :: SpecWith TestServerFull
updateSelf = describe "update self" $ do
    let _fill :: ST -> ST -> WD.WD ()
        _fill label text = WD.findElem (WD.ById label) >>= (\e -> WD.clearInput e >> WD.sendKeys text e)

        _click :: ST -> WD.WD ()
        _click label = WD.findElem (WD.ById label) >>= WD.click

        _check ::  AcidState DB -> (User -> IO ()) -> WD.WD ()
        _check st f = liftIO $ query' st (SnapShot allowEverything) >>=
                        maybe (error "no such user") f .
                        either (error "could not take db snapshot") (Map.lookup (UserId selfId) . (^. dbUsers))

        -- FIXME: test with ordinary user (not god).
        selfId   :: Integer = 0
        selfName :: ST      = "god"
        selfPass :: ST      = "god"

    it "username" $ \ (((st, _, _), _, (_, feConfig), wd) :: TestServerFull) -> wd $ do
        let newSelfName :: ST = "da39a3ee5e6b4b0d3255bfef95601890afd80709"
        wdLogin feConfig (UserName selfName) (UserPass selfPass) >>= liftIO . (`shouldBe` 200) . C.statusCode
        WD.openPage (cs $ exposeUrl feConfig <//> "/user/update")
        _fill "/user/update.name" newSelfName
        _click "update_user_submit"
        _check st ((`shouldBe` UserName newSelfName) . (^. userName))

    -- FIXME: test with new user name that is already in use.
    -- FIXME: test with unauthenticated user.
    -- FIXME: test with other user (user A wants to edit uesr B), with and without RoleAdmin.

    it "password" $ \ (((st, _, _), _, (_, feConfig), wd) :: TestServerFull) -> wd $ do
        let newSelfPass :: ST = "da39a3ee5e6b4b0d3255bfef95601890afd80709"
        wdLogin feConfig (UserName selfName) (UserPass selfPass) >>= liftIO . (`shouldBe` 200) . C.statusCode
        WD.openPage (cs $ exposeUrl feConfig <//> "/user/update_password")
        _fill "/user/update_password.old_password" selfPass
        _fill "/user/update_password.new_password1" newSelfPass
        _fill "/user/update_password.new_password2" newSelfPass
        _click "update_password_submit"
        _check st (`shouldSatisfy` verifyPass (UserPass newSelfPass))

    -- FIXME: test failure cases.  same restrictions apply as in
    -- "create_user" and "reset_password" (make sure the check is in
    -- separate function, not inlined.)

    it "email" $ \ (_ :: TestServerFull) ->
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


-- manageRoles :: SpecWith TestServerFull
-- manageRoles = describe "manage roles" $ do ...


logIntoThentos :: SpecWith TestServerFull
logIntoThentos = it "log into thentos" $ \ ((_, _, (_, feConfig), wd) :: TestServerFull) -> wd $ do
    wdLogin feConfig "god" "god" >>= liftIO . (`shouldBe` 200) . C.statusCode
    WD.getSource >>= \ s -> liftIO $ (cs s) `shouldSatisfy` (=~# "Login successful")

    -- (out of curiousity: why do we need the type signature in the
    -- lambda parameter?  shouldn't ghc infer (and be happy with the
    -- fact) that the lambda is polymorphic in all places where it
    -- takes '_'?)

logOutOfThentos :: SpecWith TestServerFull
logOutOfThentos = it "log out of thentos" $ \ ((_, _, (_, feConfig), wd) :: TestServerFull) -> wd $ do
    -- logout when logged in
    wdLogin feConfig "god" "god" >>= liftIO . (`shouldBe` 200) . C.statusCode
    wdLogout feConfig >>= liftIO . (`shouldBe` 200) . C.statusCode
    WD.getSource >>= \ s -> liftIO $ (cs s) `shouldSatisfy` (=~# "You have been logged out")

    -- logout when already logged out
    wdLogout feConfig >>= liftIO . (`shouldBe` 400) . C.statusCode


serviceCreate :: SpecWith TestServerFull
serviceCreate = it "service create" $ \ (((st, _, _), _, (_, feConfig), wd) :: TestServerFull) -> do
    -- fe: fill out and submit create-service form
    let sname :: ST = "Evil Corp."
        sdescr :: ST = "don't be evil."
    serviceId :: ServiceId <- wd $ do
        wdLogin feConfig "god" "god" >>= liftIO . (`shouldBe` 200) . C.statusCode
        WD.openPage (cs $ exposeUrl feConfig <//> "/dashboard/ownservices")

        let fill :: WD.WebDriver wd => ST -> ST -> wd ()
            fill label text = WD.findElem (WD.ById label) >>= WD.sendKeys text

        fill "/dashboard/ownservices.name" sname
        fill "/dashboard/ownservices.description" sdescr

        WD.findElem (WD.ById "create_service_submit") >>= WD.click

        (\ s -> case cs s =~- "Service id: (.+)<" of [_, sid] -> ServiceId $ cs (LB.take 24 sid)) <$> WD.getSource

    -- db: check that
    --   1. service has been created;
    --   2. has right sname, sdescr;
    --   3. has correct owner.
    Right (db :: DB) <- query' st $ SnapShot allowEverything
    case Map.lookup serviceId (db ^. dbServices) of
        Nothing -> error "serviceId not found in db."
        Just service -> do
            service ^. serviceSession     `shouldBe` Nothing
            service ^. serviceName        `shouldBe` ServiceName sname
            service ^. serviceDescription `shouldBe` ServiceDescription sdescr
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
    WD.setImplicitWait 200
    WD.openPage (cs $ exposeUrl feConfig <//> "/user/login")

    let fill :: WD.WebDriver wd => ST -> ST -> wd ()
        fill label text = WD.findElem (WD.ById label) >>= WD.sendKeys text
    fill "/user/login.name" uname
    fill "/user/login.password" upass

    WD.findElem (WD.ById "login_submit") >>= WD.click
    return $ C.Status 200 "Ok."  -- FIXME: we need a man in the middle
                                 -- between browser and http server
                                 -- that we can ask for things
                                 -- happening between the two.
                                 -- selenium doesn't allow that.

wdLogout :: HttpConfig -> WD.WD C.Status
wdLogout feConfig = do
    WD.openPage (cs $ exposeUrl feConfig <//> "/user/logout")
    WD.findElems (WD.ById "logout_submit") >>= maybe noButton buttonIsThere . listToMaybe
  where
    noButton = do
        return $ C.Status 400 "Perhaps we are already logged out?"
    buttonIsThere el = do
        WD.click el
        return $ C.Status 200 "Ok."  -- FIXME: as in wdLogin

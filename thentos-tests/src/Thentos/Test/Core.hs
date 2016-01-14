{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Thentos.Test.Core
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newMVar)
import Control.Exception (bracket, finally)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad (when)
import "cryptonite" Crypto.Random (drgNew)
import Crypto.Scrypt (Pass(Pass), EncryptedPass(..), encryptPass, Salt(Salt), scryptParams)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Configifier ((>>.))
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Pool (Pool, withResource, destroyAllResources)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs)
import Data.Void (Void)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Types.Header (Header)
import System.IO (stderr)
import System.Log.Formatter (simpleLogFormatter, nullFormatter)
import System.Log.Handler.Simple (formatter, fileHandler, streamHandler)
import System.Log.Logger (Priority(DEBUG), removeAllHandlers, updateGlobalLogger,
                          setLevel, addHandler)
import System.Process (callCommand)
import Test.Mockery.Directory (inTempDirectory)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Test.WebDriver as WD

import System.Log.Missing (loggerName)
import Thentos.Action.Core
import Thentos.Action.Types
import Thentos.Action hiding (addUser)
import Thentos.Backend.Api.Simple as Simple
import Thentos.Backend.Core
import Thentos.Config
import Thentos (createConnPoolAndInitDb)
import Thentos.Frontend (runFrontend)
import Thentos.Transaction
import Thentos.Transaction.Core
import Thentos.Types

import qualified Thentos.Action.Unsafe as U

import Thentos.Test.Config


-- * test users

testUserForms :: [UserFormData]
testUserForms =
    [ UserFormData "name1" "passwd" $ forceUserEmail "em@il.org"
    , UserFormData "name2" "passwd" $ forceUserEmail "em38@il.org"
    , UserFormData "name3" "3" $ forceUserEmail "3@example.org"
    , UserFormData "name4" "4" $ forceUserEmail "4@example.org"
    , UserFormData "name5" "5" $ forceUserEmail "5@example.org"
    ]

testUsers :: [User]
testUsers = (\ (UserFormData name pass email) ->
                User name (encryptTestSecret . cs . fromUserPass $ pass) email)
    <$> testUserForms

testUser :: User
testUser = head testUsers

testUid :: UserId
testUid = UserId 7

testHashedSecret :: HashedSecret ServiceKey
testHashedSecret = HashedSecret (EncryptedPass "afhbadigba")

-- | Add a single test user (with fast scrypt params) from 'testUsers' to the database and return
-- it.
addTestUser :: Int -> Action Void s (UserId, UserFormData, User)
addTestUser ((zip testUserForms testUsers !!) -> (uf, user)) = do
    uid <- U.unsafeAction . U.query $ addUser user
    return (uid, uf, user)

-- | Create a list of test users (with fast scrypt params), store them in the database, and return
-- them for use in test cases.
initializeTestUsers :: Action Void s [(UserId, UserFormData, User)]
initializeTestUsers = mapM addTestUser [0 .. length testUsers - 1]

encryptTestSecret :: ByteString -> HashedSecret a
encryptTestSecret pw =
    HashedSecret $
        encryptPass (fromJust $ scryptParams 2 1 1) (Salt "") (Pass pw)


-- * Sample data for making services, contexts, and personas

servId :: ServiceId
servId = "sid"

cxtName :: ContextName
cxtName = "Kiezkasse"

cxtDesc :: ContextDescription
cxtDesc = "A simple sample context"

cxtUrl :: ProxyUri
cxtUrl = ProxyUri "example.org" 80 "/kiezkasse"

persName :: PersonaName
persName = "MyOtherSelf"


-- * runners

withLogger :: IO a -> IO a
withLogger = inTempDirectory . withLogger'

-- | Run an action, logging everything with 'DEBUG' level to @./everything.log@.
withLogger' :: IO a -> IO a
withLogger' = withLogger_ False

withNoisyLogger :: IO a -> IO a
withNoisyLogger = inTempDirectory . withNoisyLogger'

-- | This is a workaround for the fact that log file contents is not included in the output of
-- failing test cases.  If you replace the call to `withLogger` with one ot `withNoisyLogger` in a
-- test, everything will go to stderr immediately.
--
-- FIXME: include log contents in failing test cases.
--
-- FIXME: while we are at it, it would be really cool (and not that hard) to provide a log handler
-- that logs into a 'Chan', and expose the Chan to the tests.  that would make it easy to use log
-- file contents to formulate tests.
withNoisyLogger' :: IO a -> IO a
withNoisyLogger' = withLogger_ True

withLogger_ :: Bool -> IO a -> IO a
withLogger_ stderrAlways action = do
    removeAllHandlers
    updateGlobalLogger loggerName $ setLevel DEBUG

    let fmt = simpleLogFormatter "$utcTime *$prio* [$pid][$tid] -- $msg"
        addh h = addHandler $ h { formatter = fmt }

    fileHandler "./everything.log" DEBUG >>= updateGlobalLogger loggerName . addh
    when stderrAlways $
        streamHandler stderr DEBUG >>= updateGlobalLogger loggerName . addh

    result <- action
    removeAllHandlers
    return result

withSignupLogger :: IO a -> IO a
withSignupLogger action = inTempDirectory $ do
    removeAllHandlers
    updateGlobalLogger signupLogger $ setLevel DEBUG
    let addh h = addHandler $ h { formatter = nullFormatter }
    fileHandler "./signups.log" DEBUG >>= updateGlobalLogger signupLogger . addh
    result <- action
    removeAllHandlers
    return result

withWebDriver :: WD.WD r -> IO r
withWebDriver = inTempDirectory . withWebDriver'

-- | Start and shutdown webdriver on localhost:4451, running the action in between.
withWebDriver' :: WD.WD r -> IO r
withWebDriver' = withWebDriverAt' "localhost" 4451

-- | Start and shutdown webdriver on the specified host and port, running the
-- action in between.
withWebDriverAt' :: String -> Int -> WD.WD r -> IO r
withWebDriverAt' host port action = WD.runSession wdConfig . WD.finallyClose $ do
     -- running `WD.closeOnException` here is not
     -- recommended, as it hides all hspec errors behind an
     -- uninformative java exception.
    WD.setImplicitWait 1000
    WD.setScriptTimeout 1000
    WD.setPageLoadTimeout 1000
    action

    where wdConfig = WD.defaultConfig
            { WD.wdHost = host
            , WD.wdPort = port
            }

withFrontend :: HttpConfig -> ActionState -> IO r -> IO r
withFrontend feConfig as = inTempDirectory . withFrontend' feConfig as

-- | Start and shutdown the frontend in the specified @HttpConfig@ and with the
-- specified DB, running an action in between.
withFrontend' :: HttpConfig -> ActionState -> IO r -> IO r
withFrontend' feConfig as action =
    bracket (forkIO $ Thentos.Frontend.runFrontend feConfig as)
            killThread
            (const action)

withBackend :: HttpConfig -> ActionState -> IO r -> IO r
withBackend beConfig as = inTempDirectory . withBackend' beConfig as

-- | Run a @hspec-wai@ @Session@ with the backend @Application@.
withBackend' :: HttpConfig -> ActionState -> IO r -> IO r
withBackend' beConfig as action = do
    bracket (forkIO $ runWarpWithCfg beConfig $ Simple.serveApi beConfig as)
            killThread
            (const action)

withFrontendAndBackend :: (ActionState -> IO r) -> IO r
withFrontendAndBackend = inTempDirectory . withFrontendAndBackend'

-- | Sets up DB, frontend and backend, creates god user, runs an action that
-- takes a DB, and tears down everything, returning the result of the action.
withFrontendAndBackend' :: (ActionState -> IO r) -> IO r
withFrontendAndBackend' test = do
    st@(ActionState cfg _ connPool) <- createActionState
    withFrontend' (getFrontendConfig cfg) st
        $ withBackend' (getBackendConfig cfg) st
            $ withResource connPool (\conn -> liftIO (createGod conn) >> test st)
                `finally` destroyAllResources connPool


-- * set up state

-- | Create an @ActionState@ with default config and a connection to a DB.  Whipes the DB.
createActionState :: IO ActionState
createActionState = thentosTestConfig >>= createActionState'

-- | Create an @ActionState@ with an explicit config and a connection to a DB.  Whipes the DB.
createActionState' :: ThentosConfig -> IO ActionState
createActionState' cfg = ActionState cfg <$> (drgNew >>= newMVar) <*> createDb cfg

-- | Create a connection to a DB.  Whipes the DB.
createDb :: ThentosConfig -> IO (Pool Connection)
createDb cfg = callCommand (create_ <> " && " <> wipe_) >> createConnPoolAndInitDb cfg
  where
    dbname  = cs $ cfg >>. (Proxy :: Proxy '["database", "name"])
    create_ = "createdb " <> dbname <> " 2>/dev/null || true"
    wipe_   = "psql --quiet --file=" <> wipeFile <> " " <> dbname <> " >/dev/null 2>&1"

loginAsGod :: ActionState -> IO (ThentosSessionToken, [Header])
loginAsGod actionState = do
    let action :: Action Void () (UserId, ThentosSessionToken)
        action = startThentosSessionByUserName godName godPass
    ((_, tok), _) <- runAction () actionState action
    let credentials :: [Header] = [(mk "X-Thentos-Session", cs $ fromThentosSessionToken tok)]
    return (tok, credentials)


-- * misc

-- | This is convenient if you have lots of string literals with @-XOverloadedStrings@ but do not
-- want to do explicit type signatures to avoid type ambiguity.
(..=) :: ST -> ST -> Aeson.Pair
(..=) = (Aeson..=)

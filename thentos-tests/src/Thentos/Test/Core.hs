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

{-# OPTIONS_GHC  #-}

module Thentos.Test.Core
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newMVar)
import Control.Exception (bracket, finally)
import Control.Monad.IO.Class (MonadIO(liftIO))
import "cryptonite" Crypto.Random (drgNew)
import Crypto.Scrypt (Salt(Salt), scryptParams)
import Data.Configifier ((>>.))
import Data.Monoid ((<>))
import Data.Pool (Pool, destroyAllResources)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs)
import Data.Void (Void)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Types.Header (Header)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Process (callCommand)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Test.Mockery.Directory (inTempDirectory)
import qualified Test.WebDriver as WD

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
import Thentos.Util (hashSecretWith)

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
                User name (encryptTestSecret fromUserPass pass) email)
    <$> testUserForms

testUser :: User
testUser = head testUsers

testUid :: UserId
testUid = UserId 7

testHashedSecret :: HashedSecret ServiceKey
testHashedSecret = SCryptHash "afhbadigba"

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

encryptTestSecret :: (a -> ST) -> a -> HashedSecret a
encryptTestSecret = hashSecretWith params salt
  where
    salt = Salt ""
    Just params = scryptParams 2 1 1


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

-- | Mockery calls 'setCurrentDirectory', which interferes with the fact that thentos allows for
-- `root_path` to be relative to the working directory from which the executable is started: Two
-- current directories break things.
--
-- This function solves this problem by leaving the current directory intact for the wrapped action,
-- and passing the temp directory in as an explicit argument.
outsideTempDirectory :: (FilePath -> IO a) -> IO a
outsideTempDirectory action = do
    wd <- getCurrentDirectory
    Test.Mockery.Directory.inTempDirectory $ do
        wd' <- getCurrentDirectory
        setCurrentDirectory wd
        action wd'

-- | Start and shutdown webdriver on localhost:4451, running the action in between.
withWebDriver :: WD.WD r -> IO r
withWebDriver = withWebDriverAt' "localhost" 4451

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

-- | Start and shutdown the frontend in the specified @HttpConfig@ and with the
-- specified DB, running an action in between.
withFrontend :: HttpConfig -> ActionState -> IO r -> IO r
withFrontend feConfig as action =
    bracket (forkIO $ Thentos.Frontend.runFrontend feConfig as)
            killThread
            (const action)

-- | Run a @hspec-wai@ @Session@ with the backend @Application@.
withBackend :: HttpConfig -> ActionState -> IO r -> IO r
withBackend beConfig as action = do
    bracket (forkIO $ runWarpWithCfg beConfig $ Simple.serveApi beConfig as)
            killThread
            (const action)

-- | Sets up DB, frontend and backend, creates god user, runs an action that
-- takes a DB, and tears down everything, returning the result of the action.
withFrontendAndBackend :: (ActionState -> IO r) -> IO r
withFrontendAndBackend test = do
    st@(ActionState cfg _ connPool) <- createActionState
    withFrontend (getFrontendConfig cfg) st
        $ withBackend (getBackendConfig cfg) st
            $ liftIO (createGod connPool) >> test st
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
createDb cfg = callCommand (createCmd <> " && " <> wipeCmd) >> createConnPoolAndInitDb cfg
  where
    dbname    = cs $ cfg >>. (Proxy :: Proxy '["database", "name"])
    createCmd = "createdb " <> dbname <> " 2>/dev/null || true"
    wipeCmd   = "psql --quiet --file=" <> wipeFile <> " " <> dbname <> " >/dev/null 2>&1"

loginAsGod :: ActionState -> IO (ThentosSessionToken, Header)
loginAsGod actionState = do
    let action :: Action Void () (UserId, ThentosSessionToken)
        action = startThentosSessionByUserName godName godPass
    ((_, tok), _) <- runAction () actionState action
    let credentials :: Header = ("X-Thentos-Session", cs $ fromThentosSessionToken tok)
    return (tok, credentials)


-- * misc

-- | This is convenient if you have lots of string literals with @-XOverloadedStrings@ but do not
-- want to do explicit type signatures to avoid type ambiguity.
(..=) :: ST -> ST -> Aeson.Pair
(..=) = (Aeson..=)

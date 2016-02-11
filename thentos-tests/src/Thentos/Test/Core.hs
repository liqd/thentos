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

{-# OPTIONS_GHC  #-}

module Thentos.Test.Core
    ( testUserForms
    , createTestUsers
    , mkUser
    , mkUser'
    , encryptTestSecret
    , testHashedServiceKey
    , testHashedUserPass
    , servId
    , cxtName
    , cxtDesc
    , cxtUrl
    , persName
    , outsideTempDirectory
    , withWebDriver
    , withWebDriverAt'
    , withFrontend
    , withBackend
    , withFrontendAndBackend
    , createActionState
    , createActionState'
    , createDb
    , loginAsDefaultUser
    , (..=)
    )
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newMVar)
import Control.Exception (bracket, finally)
import Control.Lens ((^.))
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
import System.Exit (ExitCode(ExitSuccess))
import System.Process (system)

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
import Thentos.Frontend (runFrontend)
import Thentos.Transaction
import Thentos.Transaction.Core
import Thentos.Types
import Thentos.Util (hashSecretWith)
import Thentos (createConnPoolAndInitDb, createDefaultUser)

import Thentos.Test.Config


-- * test users

testUserForms :: [UserFormData]
testUserForms = [ let n = "name" <> cs (show i)
                      p = "passwd"
                      e = n <> "@example.org"
                  in UserFormData (UserName n) (UserPass p) (forceUserEmail e)
                  | i <- [(0 :: Int) ..]
                ]

-- | Create a list of test users (with fast scrypt params), store them in the database, and return
-- them for use in test cases.
createTestUsers :: MonadIO m => Pool Connection -> Int -> m [(UserId, UserPass, User)]
createTestUsers connPool num = do
    Right users <- liftIO . runThentosQuery connPool . createTestUsers' $ num
    return users

createTestUsers' :: Int -> ThentosQuery Void [(UserId, UserPass, User)]
createTestUsers' num = mapM addTestUser (take num testUserForms)

addTestUser :: UserFormData -> ThentosQuery Void (UserId, UserPass, User)
addTestUser uf@(UserFormData _ pass _) = do
    let user = mkUser uf
    uid <- addUser user
    return (uid, pass, user)

mkUser :: UserFormData -> User
mkUser (UserFormData name pass email) = User name pass' email
  where
    pass' = encryptTestSecret fromUserPass pass

mkUser' :: UserName -> UserPass -> ST -> User
mkUser' n p e = mkUser (UserFormData n p (forceUserEmail e))

encryptTestSecret :: (a -> ST) -> a -> HashedSecret a
encryptTestSecret = hashSecretWith params salt
  where
    salt = Salt ""
    Just params = scryptParams 2 1 1

testHashedServiceKey :: HashedSecret ServiceKey
testHashedServiceKey = SCryptHash "afhbadigba"

testHashedUserPass :: HashedSecret UserPass
testHashedUserPass = SCryptHash "afhbadigba"


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
            $ liftIO (createDefaultUser st) >> test st
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
createDb cfg = do
    _           <- system createCmd
    ExitSuccess <- system wipeCmd
    createConnPoolAndInitDb cfg
  where
    verbose   = False
    stdouterr = if verbose then "" else " >/dev/null 2>/dev/null"
    dbname    = cs $ cfg >>. (Proxy :: Proxy '["database", "name"])
    createCmd = "createdb " <> dbname <> stdouterr
    wipeCmd   = "psql --file=" <> wipeFile <> " " <> dbname <> stdouterr

loginAsDefaultUser :: ActionState -> IO (ThentosSessionToken, Header)
loginAsDefaultUser actionState = do
    let action :: Action Void () (UserId, ThentosSessionToken)
        action = startThentosSessionByUserName (UserName uname) (UserPass upass)
          where
            uname, upass :: ST
            Just uname = actionState ^. aStConfig >>. (Proxy :: Proxy '["default_user", "name"])
            Just upass = actionState ^. aStConfig >>. (Proxy :: Proxy '["default_user", "password"])

    ((_, tok), _) <- runAction () actionState action
    let credentials :: Header = ("X-Thentos-Session", cs $ fromThentosSessionToken tok)
    return (tok, credentials)


-- * misc

-- | This is convenient if you have lots of string literals with @-XOverloadedStrings@ but do not
-- want to do explicit type signatures to avoid type ambiguity.
(..=) :: ST -> ST -> Aeson.Pair
(..=) = (Aeson..=)

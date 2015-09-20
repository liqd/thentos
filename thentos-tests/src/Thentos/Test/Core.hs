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
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Thentos.Test.Core
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Crypto.Random (ChaChaDRG, drgNew)
import Crypto.Scrypt (Pass(Pass), EncryptedPass(..), encryptPass, Salt(Salt), scryptParams)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Pool (withResource)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (LBS, SBS, ST, cs)
import Data.Void (Void)

import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Method (Method)
import Network.Wai (requestMethod, requestHeaders)
import Network.Wai.Test (SRequest(SRequest), setPath, defaultRequest)
import System.FilePath ((</>))
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler.Simple (formatter, fileHandler)
import System.Log.Logger (Priority(DEBUG), removeAllHandlers, updateGlobalLogger, setLevel, setHandlers)
import System.IO.Temp (createTempDirectory)
import System.Process

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.ByteString as AP
import qualified Test.WebDriver as WD

import System.Log.Missing (loggerName)
import Thentos (createConnPoolAndInitDb)
import Thentos.Action hiding (addUser)
import Thentos.Action.Core
import Thentos.Backend.Api.Simple as Simple
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Frontend (runFrontend)
import Thentos.Transaction
import Thentos.Transaction.Core
import Thentos.Types

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
addTestUser :: Int -> Action Void (UserId, UserFormData, User)
addTestUser ((zip testUserForms testUsers !!) -> (uf, user)) = do
    uid <- query'P $ addUser user
    return (uid, uf, user)

-- | Create a list of test users (with fast scrypt params), store them in the database, and return
-- them for use in test cases.
initializeTestUsers :: Action Void [(UserId, UserFormData, User)]
initializeTestUsers = mapM addTestUser [0 .. length testUsers - 1]

encryptTestSecret :: ByteString -> HashedSecret a
encryptTestSecret pw =
    HashedSecret $
        encryptPass (fromJust $ scryptParams 2 1 1) (Salt "") (Pass pw)


-- * test logger

-- | Run an action, logging everything with 'DEBUG' level to a temp file.
withLogger :: IO a -> IO a
withLogger action = do
    let loglevel = DEBUG
        fmt = simpleLogFormatter "$utcTime *$prio* [$pid][$tid] -- $msg"
    removeAllHandlers
    tmp <- createTempDirectory "/tmp/" "_thentos_test_"
    fHandler <- (\ h -> h { formatter = fmt }) <$> fileHandler (tmp </> "everything.log")  loglevel
    updateGlobalLogger loggerName $ setLevel DEBUG . setHandlers [fHandler]
    result <- action
    removeAllHandlers
    return result

-- | Start and shutdown webdriver on localhost:4451, running the action in between.
withWebDriver :: WD.WD r -> IO r
withWebDriver = withWebDriver' "localhost" 4451

-- | Start and shutdown webdriver on the specified host and port, running the
-- action in between.
withWebDriver' :: String -> Int -> WD.WD r -> IO r
withWebDriver' host port action = WD.runSession wdConfig . WD.finallyClose $ do
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
withBackend beConfig as action =
    bracket (forkIO $ runWarpWithCfg beConfig $ Simple.serveApi as)
            killThread
            (const action)

-- | Sets up DB, frontend and backend, creates god user, runs an action that
-- takes a DB, and tears down everything, returning the result of the action.
withFrontendAndBackend :: String -> (ActionState -> IO r) -> IO r
withFrontendAndBackend dbname test = do
    st@(ActionState (connPool, _, _)) <- createActionState dbname thentosTestConfig
    withFrontend defaultFrontendConfig st
        $ withBackend defaultBackendConfig st
            $ withResource connPool $ \conn -> liftIO (createGod conn) >> test st

defaultBackendConfig :: HttpConfig
defaultBackendConfig = fromJust $ Tagged <$> thentosTestConfig >>. (Proxy :: Proxy '["backend"])

defaultFrontendConfig :: HttpConfig
defaultFrontendConfig = fromJust $ Tagged <$> thentosTestConfig >>. (Proxy :: Proxy '["frontend"])

-- | Create an @ActionState@ with a connection to an empty DB and the specified
-- config.
createActionState :: String -> ThentosConfig -> IO ActionState
createActionState dbname config = do
    rng :: MVar ChaChaDRG <- drgNew >>= newMVar
    wipe <- wipeFile
    callCommand $ "createdb " <> dbname <> " 2>/dev/null || true"
               <> " && psql --quiet --file=" <> wipe <> " " <> dbname <> " >/dev/null 2>&1"
    connPool <- createConnPoolAndInitDb $ cs dbname
    return $ ActionState (connPool, rng, config)

loginAsGod :: ActionState -> IO (ThentosSessionToken, [Header])
loginAsGod actionState = do
    (_, tok) <- runAction actionState $ (startThentosSessionByUserName godName godPass :: Action Void (UserId, ThentosSessionToken))
    let credentials :: [Header] = [(mk "X-Thentos-Session", cs $ fromThentosSessionToken tok)]
    return (tok, credentials)


-- | Cloned from hspec-wai's 'request'.  (We don't want to use the
-- return type from there.)
makeSRequest :: Method -> SBS -> [Header] -> LBS -> SRequest
makeSRequest method path headers = SRequest req
  where
    req = setPath defaultRequest { requestMethod = method, requestHeaders = headers ++ defaultHeaders } path
    defaultHeaders = [("Content-Type", "application/json")]


-- * misc

-- | Like 'Data.Aeson.decode' but allows all JSON values instead of just
-- objects and arrays.
--
-- Copied from https://github.com/haskell-servant/servant-client
-- (FIXME: also available from attoparsec these days.  replace!)
decodeLenient :: Aeson.FromJSON a => LBS -> Either String a
decodeLenient input = do
    v :: Aeson.Value <- AP.parseOnly (Aeson.value <* AP.endOfInput) (cs input)
    Aeson.parseEither Aeson.parseJSON v

-- | This is convenient if you have lots of string literals with @-XOverloadedStrings@ but do not
-- want to do explicit type signatures to avoid type ambiguity.
(..=) :: ST -> ST -> Aeson.Pair
(..=) = (Aeson..=)

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
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

import Control.Applicative ((<*), (<$>))
import Control.Concurrent.Async (Async, async, cancel, poll)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (Exception, SomeException, throwIO, catch)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Crypto.Random (ChaChaDRG, drgNew)
import Crypto.Scrypt (Pass(Pass), encryptPass, Salt(Salt), scryptParams)
import Data.Acid (IsAcidic)
import Data.Acid.Memory (openMemoryState)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Configifier ((>>.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (LBS, SBS, ST, cs)
import Data.Typeable (Typeable)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Method (Method)
import Network (HostName, PortID(PortNumber), connectTo)
import Network.Wai (Application, StreamingBody, Request, requestMethod, requestBody, strictRequestBody, requestHeaders)
import Network.Wai.Internal (Response(ResponseFile, ResponseBuilder, ResponseStream, ResponseRaw))
import Network.Wai.Test (SRequest(SRequest), setPath, defaultRequest)
import System.IO (hClose)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler.Simple (formatter, fileHandler)
import System.Log.Logger (Priority(DEBUG), removeAllHandlers, updateGlobalLogger, setLevel, setHandlers)
import System.Timeout (timeout)
import Text.Show.Pretty (ppShow)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Test.WebDriver as WD

import System.Log.Missing (logger, loggerName)
import Thentos.Action
import Thentos.Action.Core
import Thentos.Backend.Api.Simple as Simple
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Frontend (runFrontend)
import Thentos.Transaction hiding (addService)
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
                User name (encryptTestSecret . cs . fromUserPass $ pass) email Set.empty Map.empty)
    <$> testUserForms

-- | Add a single test user (with fast scrypt params) from 'testUsers' to the database and return
-- it.
addTestUser :: Int -> Action DB (UserId, UserFormData, User)
addTestUser ((zip testUserForms testUsers !!) -> (uf, user)) = do
    uid <- update'P $ AddUser user
    return (uid, uf, user)

-- | Create a list of test users (with fast scrypt params), store them in the database, and return
-- them for use in test cases.
initializeTestUsers :: Action DB [(UserId, UserFormData, User)]
initializeTestUsers = mapM addTestUser [0 .. length testUsers - 1]

encryptTestSecret :: ByteString -> HashedSecret a
encryptTestSecret pw =
    HashedSecret $
        encryptPass (fromJust $ scryptParams 2 1 1) (Salt "") (Pass pw)


-- * test logger

-- | Run an action, logging everything with 'DEBUG' level to the specified file.
withLogger :: FilePath -> IO a -> IO a
withLogger logfile action = do
    let loglevel = DEBUG
        fmt = simpleLogFormatter "$utcTime *$prio* [$pid][$tid] -- $msg"
    removeAllHandlers
    fHandler <- (\ h -> h { formatter = fmt }) <$> fileHandler logfile loglevel
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
withFrontend :: MonadIO m => HttpConfig -> ActionState DB -> m r -> m r
withFrontend feConfig as action = do
    fe <- liftIO . async $ Thentos.Frontend.runFrontend feConfig as
    liftIO $ assertResponsive 1.5 [(fe, feConfig)]
    result <- action
    liftIO $ cancel fe
    return result

defaultFrontendConfig :: HttpConfig
defaultFrontendConfig = undefined -- [NOPUSH]

defaultBackendConfig :: HttpConfig
defaultBackendConfig = undefined -- [NOPUSH]

-- | Sets up DB, frontend and backend, runs an action that takes a DB, and
-- tears down everything, returning the result of the action.
withFrontendAndBackend :: MonadIO m => (ActionState DB -> m r) -> m r
withFrontendAndBackend test = do
    st <- liftIO $ createActionState undefined -- [NOPUSH]
    withFrontend defaultFrontendConfig st
       $ withBackend defaultBackendConfig st
       $ test st

-- | Run a @hspec-wai@ @Session@ with the backend @Application@.
withBackend :: MonadIO m => HttpConfig -> ActionState DB -> m r -> m r
withBackend beConfig as action = do
    backend <- liftIO . async $ runWarpWithCfg beConfig $ Simple.serveApi as
    liftIO $ assertResponsive 1.5 [(backend, beConfig)]
    result <- action
    liftIO $ cancel backend
    return result


{-
withBackend  :: Application -> IO r -> IO r

-- | Run an action with a database, frontend and backend servers, in the way it
-- would be run given the specified config file.
withConfigured :: String -> IO r -> IO r
-}

-- | Create an @ActionState@ with an empty in-memory DB and the specified
-- config.
createActionState :: (EmptyDB db, IsAcidic db) => ThentosConfig -> IO (ActionState db)
createActionState config = do
    rng :: MVar ChaChaDRG <- drgNew >>= newMVar
    st <- openMemoryState emptyDB
    return $ ActionState (st, rng, config)


-- * FTS

-- | Set up both frontend and backend on real tcp sockets (introduced
-- for webdriver testing, but may be used elsewhere).
{-
setupTestServerFull :: IO (FTS DB)
setupTestServerFull = do
    DBTS tcfg asg <- setupDB

    let Just (beConfig :: HttpConfig) = Tagged <$> testThentosConfig tcfg >>. (Proxy :: Proxy '["backend"])
        Just (feConfig :: HttpConfig) = Tagged <$> testThentosConfig tcfg >>. (Proxy :: Proxy '["frontend"])

    backend  <- async $ runWarpWithCfg beConfig . tracifyApplication tcfg $ Simple.serveApi asg
    frontend <- async $ Thentos.Frontend.runFrontend feConfig asg

    assertResponsive 1.5 [(backend, beConfig), (frontend, feConfig)]

    let wdConfig = WD.defaultConfig
            { WD.wdHost = tcfg ^. tcfgWebdriverHost
            , WD.wdPort = tcfg ^. tcfgWebdriverPort
            }

        wd :: forall a . WD.WD a -> IO a
        wd action = WD.runSession wdConfig . WD.finallyClose $ do
             -- running `WD.closeOnException` here is not
             -- recommended, as it hides all hspec errors behind an
             -- uninformative java exception.
            WD.setImplicitWait 1000
            WD.setScriptTimeout 1000
            WD.setPageLoadTimeout 1000
            action

    return $ FTS tcfg asg backend beConfig frontend feConfig wd


teardownTestServerFull :: forall db . (db `Ex` DB) => FTS db -> IO ()
teardownTestServerFull (FTS tcfg db backend _ frontend _ _) = do
    cancel backend
    cancel frontend
    teardownDB $ DBTS tcfg db
-}

loginAsGod :: forall db . (db `Ex` DB) => ActionState db -> IO (ThentosSessionToken, [Header])
loginAsGod actionState = do
    (_, tok :: ThentosSessionToken) <- runAction actionState $ startThentosSessionByUserName godName godPass
    let credentials :: [Header] = [(mk "X-Thentos-Session", cs $ fromThentosSessionToken tok)]
    return (tok, credentials)


-- | Cloned from hspec-wai's 'request'.  (We don't want to use the
-- return type from there.)
makeSRequest :: Method -> SBS -> [Header] -> LBS -> SRequest
makeSRequest method path headers = SRequest req
  where
    req = setPath defaultRequest { requestMethod = method, requestHeaders = headers ++ defaultHeaders } path
    defaultHeaders = [("Content-Type", "application/json")]



-- | Log all requests and responses to log file.
tracifyApplication :: Application -> Application
tracifyApplication application = application'
  where
    application' :: Application
    application' req respond = do
        req' <- logRq req
        application req' $ \ rsp -> logRsp rsp >> respond rsp

    logRq :: Request -> IO Request
    logRq req = do
        bodyRef :: IORef Bool <- newIORef False
        body :: LBS <- strictRequestBody req

        logger DEBUG $ unlines
            [ ""
            , "=== REQUEST =========================================================="
            , ppShow req
            , "body:"
            , cs body
            ]

        -- It is a bit tricky to force the 'LBS' body builder without falling into a black hole.
        let memoBody = do
              toggle <- readIORef bodyRef
              writeIORef bodyRef $ not toggle
              return $ if toggle then "" else cs body

        return $ req { requestBody = memoBody }

    logRsp :: Response -> IO ()
    logRsp rsp = logger DEBUG $ unlines
        [ ""
        , "=== RESPONSE ========================================================="
        , show_ rsp
        ]
      where
        show_ :: Response -> String
        show_ (ResponseFile _ _ _ _) = "ResponseFile"
        show_ (ResponseBuilder status headers _) = "ResponseBuilder " ++ ppShow (status, headers)
        show_ (ResponseStream status headers (_ :: StreamingBody)) = "ResponseStream " ++ ppShow (status, headers)
        show_ (ResponseRaw _ _) = "ResponseRaw"


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


-- | Take a timeout (in miliseconds) and a list of 'Async' threads with corresponding 'HttpConfig's.
-- If a thread terminates, throw an error that contains either exception thrown by the thread or the
-- result value.  If it does not, connect to the associated 'HttpConfig' URI.  If that works, remove
-- thread from input list If it does not, append the thread to the end of the list.  Iterate.
assertResponsive :: Double -> [(Async (), HttpConfig)] -> IO ()
assertResponsive (round . (*1000) . (*1000) -> musecs) threads =
    timeout musecs (f threads)
        >>= maybe (throwIO $ AssertResponsiveTimeout musecs) return
  where
    f :: [(Async (), HttpConfig)] -> IO ()
    f [] = return ()
    f (x@(thread, cfg) : xs) = do
        result :: Maybe (Either SomeException ()) <- poll thread

        let h :: HostName = cs $ cfg >>. (Proxy :: Proxy '["bind_host"])
            p = PortNumber . fromIntegral $ cfg >>. (Proxy :: Proxy '["bind_port"])

        case result of
            Nothing -> do
                -- server thread is running --> attempt to connect
                isUp <- (connectTo h p >>= hClose          >> return True)
                            `catch` (\(_ :: SomeException) -> return False)
                logger DEBUG $ show (h, p, isUp)
                if isUp
                    then f xs           -- success
                    else f (xs ++ [x])  -- nothing yet, retry later
            Just r -> do
                -- server thread terminated --> crash
                throwIO . AssertResponsiveThreadTerminated $ (cfg, r)

data AssertResponsiveFailed =
      AssertResponsiveTimeout Int
    | AssertResponsiveThreadTerminated (HttpConfig, Either SomeException ())
  deriving (Show, Typeable)

instance Exception AssertResponsiveFailed


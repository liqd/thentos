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
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Test.Core
where

import Control.Applicative ((<*), (<$>))
import Control.Concurrent.Async (Async, async, cancel, poll)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (Exception, SomeException, throwIO, catch)
import Control.Lens ((^.))
import Crypto.Random (ChaChaDRG, drgNew)
import Crypto.Scrypt (Pass(Pass), encryptPass, Salt(Salt), scryptParams)
import Data.Acid (openLocalStateFrom, closeAcidState)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Configifier ((>>.), Tagged(Tagged))
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
import Network.Wai.Test (Session, SRequest(SRequest), runSession, setPath, defaultRequest)
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((</>))
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
import Thentos.Backend.Api.Adhocracy3 as Adhocracy3
import Thentos.Backend.Api.Simple as Simple
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Frontend (runFrontend)
import Thentos.Transaction
import Thentos.Types

import Test.Config
import Test.Types


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

-- | Log everything with 'DEBUG' level to @[tmp].../everything.log@.
setupLogger :: TestConfig -> IO ()
setupLogger tcfg = do
    let loglevel = DEBUG
        logfile = tcfg ^. tcfgTmp </> "everything.log"

    removeAllHandlers
    let fmt = simpleLogFormatter "$utcTime *$prio* [$pid][$tid] -- $msg"
    fHandler <- (\ h -> h { formatter = fmt }) <$> fileHandler logfile loglevel
    updateGlobalLogger loggerName $
        setLevel DEBUG . setHandlers [fHandler]

teardownLogger :: IO ()
teardownLogger = removeAllHandlers


-- * TS

setupBare :: IO TS
setupBare = do
    tcfg <- testConfig
    setupLogger tcfg
    return $ TS tcfg

teardownBare :: TS -> IO ()
teardownBare (TS tcfg) = do
    teardownLogger
    removeDirectoryRecursive (tcfg ^. tcfgTmp)


-- * DBTS

setupDB :: IO DBTS
setupDB = do
    TS tcfg <- setupBare
    st <- openLocalStateFrom (tcfg ^. tcfgDbPath) emptyDB
    createGod st
    rng :: MVar ChaChaDRG <- drgNew >>= newMVar
    return $ DBTS tcfg (ActionState (st, rng, testThentosConfig tcfg))

teardownDB :: DBTS -> IO ()
teardownDB (DBTS tcfg (ActionState (st, _, _))) = do
    closeAcidState st
    teardownBare (TS tcfg)


-- * BTS

-- | Test backend does not open a tcp socket, but uses hspec-wai
-- instead.  Comes with a session token and authentication headers
-- headers for default god user.
setupTestBackend :: Command -> IO BTS
setupTestBackend cmd = do
    DBTS tcfg asg <- setupDB
    (tok, headers) <- loginAsGod asg
    let testBackend = case cmd of
          Run   -> Simple.serveApi asg
          RunA3 -> Adhocracy3.serveApi asg
          bad -> error $ "setupTestBackend: bad command: " ++ show bad
    return $ BTS tcfg asg (tracifyApplication tcfg testBackend) tok headers

teardownTestBackend :: BTS -> IO ()
teardownTestBackend bts = teardownDB $ DBTS (bts ^. btsCfg) (bts ^. btsActionState)

runTestBackend :: BTS -> Session a -> IO a
runTestBackend bts session = runSession session (bts ^. btsWai)


-- * FTS

-- | Set up both frontend and backend on real tcp sockets (introduced
-- for webdriver testing, but may be used elsewhere).
setupTestServerFull :: IO FTS
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

teardownTestServerFull :: FTS -> IO ()
teardownTestServerFull (FTS tcfg db backend _ frontend _ _) = do
    cancel backend
    cancel frontend
    teardownDB $ DBTS tcfg db


loginAsGod :: ActionState DB -> IO (ThentosSessionToken, [Header])
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



-- | Log all requests and responses to log file (unless 'tcfgTraceApplication' of current config is
-- 'False').
tracifyApplication :: TestConfig -> Application -> Application
tracifyApplication ((^. tcfgTraceHttp) -> False) application = application
tracifyApplication _ application = application'
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

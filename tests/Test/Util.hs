{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS  #-}

module Test.Util
where

import Control.Applicative ((<*))
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad (when, void)
import Crypto.Random (SystemRNG, createEntropyPool, cprgCreate)
import Crypto.Scrypt (Pass(Pass), encryptPass, Salt(Salt), scryptParams)
import Data.Acid (AcidState, openLocalStateFrom, closeAcidState)
import Data.Acid.Advanced (update')
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.String.Conversions (LBS, SBS, cs)
import Filesystem (isDirectory, removeTree)
import GHC.Exts (fromString)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai (Application, StreamingBody, requestMethod, requestBody, strictRequestBody, requestHeaders)
import Network.Wai.Internal (Response(ResponseFile, ResponseBuilder, ResponseStream, ResponseRaw))
import Network.Wai.Test (runSession, setPath, defaultRequest, srequest, simpleBody, simpleStatus)
import Network.Wai.Test (Session, SRequest(SRequest))
import System.FilePath ((</>))
import Text.Show.Pretty (ppShow)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.ByteString as AP
import qualified Test.WebDriver as WebDriver

import Thentos.Api
import Thentos.Backend.Api.Simple
import Thentos.Config
import Thentos.DB
import Thentos.Frontend
import Thentos.Types

import Test.Config

encryptTestSecret :: ByteString -> HashedSecret a
encryptTestSecret pw =
    HashedSecret $
        encryptPass (fromJust $ scryptParams 2 1 1) (Salt "") (Pass pw)

user1, user2, user3, user4, user5 :: User
user1 = User "name1" (encryptTestSecret "passwd") "em@il" [] Nothing []
user2 = User "name2" (encryptTestSecret "passwd") "em38@il" [("bal", ["group1"]), ("bla", ["group2"])] Nothing []
user3 = User "name3" (encryptTestSecret "3") "3" [("bla", ["23"])] Nothing []
user4 = User "name4" (encryptTestSecret "4") "4" [] Nothing []
user5 = User "name5" (encryptTestSecret "5") "5" [] Nothing []


godUid :: UserId
godUid = UserId 0

godName :: UserName
godName = "god"

godPass :: UserPass
godPass = "god"

createGod :: AcidState DB -> IO ()
createGod st = createDefaultUser st
    (Just (UserFormData godName godPass "postmaster@localhost", [RoleAdmin]))


setupDB :: ThentosConfig -> IO (ActionStateGlobal (MVar SystemRNG))
setupDB thentosConfig = do
    destroyDB
    st <- openLocalStateFrom (dbPath config) emptyDB
    createGod st
    Right (UserId 1) <- update' st $ AddUser user1 allowEverything
    Right (UserId 2) <- update' st $ AddUser user2 allowEverything
    rng :: MVar SystemRNG <- createEntropyPool >>= newMVar . cprgCreate
    return (st, rng, thentosConfig)

teardownDB :: (ActionStateGlobal (MVar SystemRNG)) -> IO ()
teardownDB (st, _, _) = do
    closeAcidState st
    destroyDB

destroyDB :: IO ()
destroyDB = do
    let p = (fromString (dbPath config))
        in isDirectory p >>= \ yes -> when yes $ removeTree p

-- | Test backend does not open a tcp socket, but uses hspec-wai
-- instead.  Comes with a session token and authentication headers
-- headers for default god user.
setupTestBackend :: IO (ActionStateGlobal (MVar SystemRNG), Application, SessionToken, [Header])
setupTestBackend = do
    asg <- setupDB emptyThentosConfig
    let testBackend = serveApi asg
    (tok, headers) <- loginAsGod testBackend
    return (asg, testBackend, tok, headers)

teardownTestBackend :: (ActionStateGlobal (MVar SystemRNG), Application, SessionToken, [Header]) -> IO ()
teardownTestBackend (db, testBackend, tok, godCredentials) = do
    logoutAsGod testBackend tok godCredentials
    teardownDB db

type TestServerFull =
    ( ActionStateGlobal (MVar SystemRNG)
    , (Async (), Int)
    , (Async (), Int)
    , String -> String
    , WebDriver.WD () -> IO ()
    )

-- | Set up both frontend and backend on real tcp sockets (introduced
-- for webdriver testing, but may be used elsewhere).
setupTestServerFull :: IO TestServerFull
setupTestServerFull = do
    let cfg = emptyThentosConfig
                { frontendConfig = Just $ FrontendConfig fport
                , backendConfig = Just $ BackendConfig bport
                }

        bport = serverFullBackendPort config
        fport = serverFullFrontendPort config
        fhost = "localhost"

    asg <- setupDB cfg
    backend  <- async $ Thentos.Backend.Api.Simple.runBackend bport asg
    frontend <- async $ Thentos.Frontend.runFrontend fhost fport asg

    let wdConfig = WebDriver.defaultConfig { WebDriver.wdHost = webdriverHost config, WebDriver.wdPort = webdriverPort config }
        wd = WebDriver.runSession wdConfig . WebDriver.finallyClose . WebDriver.closeOnException
        mkUrl path = "http://" <> cs fhost <> ":" <> show fport <> "/" </> path
    return (asg, (backend, bport), (frontend, fport), mkUrl, wd)

teardownTestServerFull :: TestServerFull -> IO ()
teardownTestServerFull (db, (backend, _), (frontend, _), _, _) = do
    cancel backend
    cancel frontend
    teardownDB db

loginAsGod :: Application -> IO (SessionToken, [Header])
loginAsGod testBackend = debugRunSession False testBackend $ do
    response <- srequest (makeSRequest "POST" "/session" [] $ Aeson.encode (godUid, godPass))
    if (statusCode (simpleStatus response) /= 201)
        then error $ ppShow response
        else do
            let Just (tok :: SessionToken) = Aeson.decode' $ simpleBody response
            let credentials :: [Header] = [(mk "X-Thentos-Session", cs $ fromSessionToken tok)]
            return (tok, credentials)

logoutAsGod :: Application -> SessionToken -> [Header] -> IO ()
logoutAsGod testBackend tok godCredentials = debugRunSession False testBackend $ do
    void . srequest . makeSRequest "DELETE" "/session" godCredentials $ Aeson.encode tok

-- | Cloned from hspec-wai's 'request'.  (We don't want to use the
-- return type from there.)
makeSRequest :: Method -> SBS -> [Header] -> LBS -> SRequest
makeSRequest method path headers body = SRequest req body
  where
    req = setPath defaultRequest { requestMethod = method, requestHeaders = headers } path

-- | Like `runSession`, but with re-ordered arguments, and with an
-- extra debug-output flag.  It's not a pretty function, but it helps
-- with debugging, and it is not intended for production use.
debugRunSession :: Bool -> Application -> Network.Wai.Test.Session a -> IO a
debugRunSession debug application session = runSession session (wrapApplication debug)
  where
    wrapApplication :: Bool -> Application
    wrapApplication False = application
    wrapApplication True = \ _request respond -> do
        (requestRendered, request') <- showRequest _request
        print requestRendered
        application request' (\ response -> putStrLn (showResponse response)  >> respond response)

    showRequest _request = do
        body :: LBS <- strictRequestBody _request
        bodyRef :: IORef Bool <- newIORef False

        let memoBody = do
              toggle <- readIORef bodyRef
              writeIORef bodyRef $ not toggle
              return $ if toggle then "" else cs body

        let  showRequestHeader = "\n=== REQUEST ==========================================================\n"

             showBody :: String
             showBody = showRequestHeader ++ ppShow _request ++ "\nbody:" ++ show body ++ "\n"

             request' = _request { requestBody = memoBody }

        return (showBody, request')
      where

    showResponse response = showResponseHeader ++ show_ response
      where
        showResponseHeader = "\n=== RESPONSE =========================================================\n"

        show_ :: Response -> String
        show_ (ResponseFile _ _ _ _) = "ResponseFile"
        show_ (ResponseBuilder status headers _) = "ResponseBuilder" ++ show (status, headers)
        show_ (ResponseStream status headers (_ :: StreamingBody)) = "ResponseStream" ++ show (status, headers)
        show_ (ResponseRaw _ _) = "ResponseRaw"


-- | Like 'Data.Aeson.decode' but allows all JSON values instead of just
-- objects and arrays.
--
-- Copied from https://github.com/haskell-servant/servant-client
-- (FIXME: also available from attoparsec these days.  replace!)
decodeLenient :: Aeson.FromJSON a => LBS -> Either String a
decodeLenient input = do
    v :: Aeson.Value <- AP.parseOnly (Aeson.value <* AP.endOfInput) (cs input)
    Aeson.parseEither Aeson.parseJSON v

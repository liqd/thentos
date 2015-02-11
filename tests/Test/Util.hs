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
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad (when)
import Crypto.Random (SystemRNG, createEntropyPool, cprgCreate)
import Crypto.Scrypt (Pass(Pass), encryptPass, Salt(Salt), scryptParams)
import Data.Acid (AcidState, openLocalStateFrom, closeAcidState)
import Data.Acid.Advanced (update')
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.String.Conversions (LBS, SBS, cs)
import Filesystem (isDirectory, removeTree)
import GHC.Exts (fromString)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Method (Method)
import Network.Wai (Application, StreamingBody, requestMethod, requestBody, strictRequestBody, requestHeaders)
import Network.Wai.Internal (Response(ResponseFile, ResponseBuilder, ResponseStream, ResponseRaw))
import Network.Wai.Test (runSession, setPath, defaultRequest)
import Network.Wai.Test (Session, SRequest(SRequest))
import Text.Show.Pretty (ppShow)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.ByteString as AP

import Backend.Api.Simple
import Config
import DB
import Api
import Types

import Test.Config

encryptTestPassword :: ByteString -> EncryptedPass
encryptTestPassword pw =
    EncryptedPass $
        encryptPass (fromJust $ scryptParams 4 4 1) (Salt "") (Pass pw)

user1, user2, user3, user4, user5 :: User
user1 = User "name1" (encryptTestPassword "passwd") "em@il" [] Nothing []
user2 = User "name2" (encryptTestPassword "passwd") "em38@il" [("bal", ["group1"]), ("bla", ["group2"])] Nothing []
user3 = User "name3" (encryptTestPassword "3") "3" [("bla", ["23"])] Nothing []
user4 = User "name4" (encryptTestPassword "4") "4" [] Nothing []
user5 = User "name5" (encryptTestPassword "5") "5" [] Nothing []


godCredentials :: [Header]
godCredentials = [("X-Thentos-User", "god"), ("X-Thentos-Password", "god")]

createGod :: AcidState DB -> IO ()
createGod st = createDefaultUser st
    (Just (UserFormData "god" "god" "postmaster@localhost", [RoleAdmin]))


setupDB :: IO (ActionStateGlobal (MVar SystemRNG))
setupDB = do
  destroyDB
  st <- openLocalStateFrom (dbPath config) emptyDB
  createGod st
  Right (UserId 1) <- update' st $ AddUser user1 allowEverything
  Right (UserId 2) <- update' st $ AddUser user2 allowEverything
  rng :: MVar SystemRNG <- createEntropyPool >>= newMVar . cprgCreate
  return (st, rng, emptyThentosConfig)

teardownDB :: (ActionStateGlobal (MVar SystemRNG)) -> IO ()
teardownDB (st, _, _) = do
  closeAcidState st
  destroyDB

destroyDB :: IO ()
destroyDB = do
  let p = (fromString (dbPath config))
    in isDirectory p >>= \ yes -> when yes $ removeTree p

setupTestServer :: IO (ActionStateGlobal (MVar SystemRNG), Application)
setupTestServer = do
  asg <- setupDB
  return (asg, serveApi asg)

teardownTestServer :: (ActionStateGlobal (MVar SystemRNG), Application) -> IO ()
teardownTestServer (db, _) = teardownDB db

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
decodeLenient :: Aeson.FromJSON a => LBS -> Either String a
decodeLenient input = do
  v :: Aeson.Value <- AP.parseOnly (Aeson.value <* AP.endOfInput) (cs input)
  Aeson.parseEither Aeson.parseJSON v

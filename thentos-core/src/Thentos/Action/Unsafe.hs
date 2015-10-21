{-# LANGUAGE Unsafe                      #-}

{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE InstanceSigs                #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE PackageImports              #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE UndecidableInstances        #-}

module Thentos.Action.Unsafe
where

import Control.Concurrent (modifyMVar)
import Control.Exception (throwIO, ErrorCall(..))
import Control.Monad.Except (throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import "cryptonite" Crypto.Random (ChaChaDRG, DRG(randomBytesGenerate))
import Data.Configifier (Tagged(Tagged), (>>.))
import Data.Pool (withResource)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, SBS)
import System.Log (Priority(DEBUG, CRITICAL))

import qualified Data.Thyme as Thyme

import Thentos.Action.Core
import Thentos.Action.SimpleAuth
import Thentos.Config
import Thentos.Smtp as TS
import Thentos.Transaction.Core (ThentosQuery, runThentosQuery)
import Thentos.Types
import Thentos.Util as TU

import qualified System.Log.Missing as SLM


query :: ThentosQuery e v -> UnsafeAction e s v
query u = do
    ActionState (connPool, _, _) <- UnsafeAction ask
    liftIO (withResource connPool (`runThentosQuery` u)) >>= either throwError return

getConfig :: UnsafeAction e s ThentosConfig
getConfig = (\(ActionState (_, _, c)) -> c) <$> UnsafeAction ask

getCurrentTime :: UnsafeAction e s Timestamp
getCurrentTime = Timestamp <$> liftIO Thyme.getCurrentTime

-- | A relative of 'cprgGenerate' from crypto-random.
genRandomBytes :: Int -> UnsafeAction e s SBS
genRandomBytes i = do
    let f :: ChaChaDRG -> (ChaChaDRG, SBS)
        f r = case randomBytesGenerate i r of (output, r') -> (r', output)
    ActionState (_, mr, _) <- UnsafeAction ask
    liftIO . modifyMVar mr $ return . f

makeUserFromFormData :: UserFormData -> UnsafeAction e s User
makeUserFromFormData = liftIO . TU.makeUserFromFormData

hashUserPass :: UserPass -> UnsafeAction e s (HashedSecret UserPass)
hashUserPass = liftIO . TU.hashUserPass

hashServiceKey :: ServiceKey -> UnsafeAction e s (HashedSecret ServiceKey)
hashServiceKey = liftIO . TU.hashServiceKey

sendMail :: Maybe UserName -> UserEmail -> ST -> ST -> UnsafeAction e s ()
sendMail mName address subject msg = do
    config <- (\(ActionState (_, _, c)) -> Tagged $ c >>. (Proxy :: Proxy '["smtp"])) <$> ask
    result <- liftIO $ TS.sendMail config mName address subject msg
    case result of
        Right () -> return ()
        Left (SendmailError s) -> liftIO $ do
            SLM.logger CRITICAL $ "error sending mail: " ++ s
            throwIO $ ErrorCall "error sending email"

logger :: Priority -> String -> UnsafeAction e s ()
logger prio = liftIO . SLM.logger prio

logIfError :: (Show e) => UnsafeAction e s v -> UnsafeAction e s v
logIfError = (`catchError` f)
  where
    f e = do
        logger DEBUG $ "*** error: " ++ show e
        throwError e

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


query :: ThentosQuery e v -> UnsafeAction e v
query u = do
    ActionState (connPool, _, _) <- UnsafeAction ask
    liftIO (withResource connPool (`runThentosQuery` u)) >>= either throwError return

getConfig :: UnsafeAction e ThentosConfig
getConfig = (\(ActionState (_, _, c)) -> c) <$> UnsafeAction ask

getCurrentTime :: UnsafeAction e Timestamp
getCurrentTime = Timestamp <$> liftIO Thyme.getCurrentTime

-- | A relative of 'cprgGenerate' from crypto-random.
genRandomBytes :: Int -> UnsafeAction e SBS
genRandomBytes i = do
    let f :: ChaChaDRG -> (ChaChaDRG, SBS)
        f r = case randomBytesGenerate i r of (output, r') -> (r', output)
    ActionState (_, mr, _) <- UnsafeAction ask
    liftIO . modifyMVar mr $ return . f

makeUserFromFormData :: UserFormData -> UnsafeAction e User
makeUserFromFormData = liftIO . TU.makeUserFromFormData

hashUserPass :: UserPass -> UnsafeAction e (HashedSecret UserPass)
hashUserPass = liftIO . TU.hashUserPass

hashServiceKey :: ServiceKey -> UnsafeAction e (HashedSecret ServiceKey)
hashServiceKey = liftIO . TU.hashServiceKey

sendMail :: SmtpConfig -> Maybe UserName -> UserEmail -> ST -> ST -> UnsafeAction e ()
sendMail config mName address subject msg = liftIO $ do
    result <- TS.sendMail config mName address subject msg
    case result of
        Right () -> return ()
        Left (SendmailError s) -> do
            SLM.logger CRITICAL $ "error sending mail: " ++ s
            throwIO $ ErrorCall "error sending email"

logger :: Priority -> String -> UnsafeAction e ()
logger prio = liftIO . SLM.logger prio

-- | (This type signature could be greatly simplified, but that would also make it less explanatory.)
logIfError :: forall m l e v f
    . (m ~ UnsafeAction f, Monad m, MonadLIO l m, MonadError e m, Show e, Show f)
    => m v -> m v
logIfError = (`catchError` f)
  where
    f e = do
        logger DEBUG $ "*** error: " ++ show e
        throwError e

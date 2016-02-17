{-# LANGUAGE Unsafe                      #-}

{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE PackageImports              #-}
{-# LANGUAGE ScopedTypeVariables         #-}

module Thentos.Action.TCB
where

import Control.Concurrent (modifyMVar)
import "cryptonite" Crypto.Random (ChaChaDRG, DRG(randomBytesGenerate))
import Data.Configifier (Tagged(Tagged), (>>.), Sel, ToValE, Exc(Done), ToConfigCode)
import LIO.TCB (ioTCB)
import Network.HostAddr (hostAddr, getHostAddr)
import System.Log.Logger (logM)
import Text.Hastache (MuConfig(..), MuContext, defaultConfig, emptyEscape, hastacheStr)

import qualified Data.Thyme as Thyme

import Thentos.Action.Types
import Thentos.Config (ThentosConfig, ThentosConfig', signupLogger)
import Thentos.Prelude
import Thentos.Types
import Thentos.Backend.Api.Auth.Types (ThentosAuthCredentials(ThentosAuthCredentials), PrivilegedIP(PrivilegedIP))

import qualified Blaze.ByteString.Builder as Builder
import qualified Data.Csv.Builder as CsvBuilder
import qualified Thentos.Action.Unsafe as U
import qualified Thentos.Smtp as TS
import qualified Thentos.Transaction as T
import qualified Thentos.Util as TU
import qualified System.Log.Missing as SLM

-- | While getting the config does not involve the LIO monad, accessing the config should be done
-- with care as it contains some sensitive bits.
getConfig :: MonadThentosReader m => m ThentosConfig
getConfig = (^. aStConfig) <$> ask

getConfigField :: (MonadThentosReader m,
                   cfg ~ ToConfigCode ThentosConfig',
                   Sel cfg ps,
                   ToValE cfg ps ~ 'Done r)
               => Proxy ps -> m r
getConfigField p = (>>. p) <$> getConfig

getCurrentTime :: MonadThentosIO m => m Timestamp
getCurrentTime = Timestamp <$> liftLIO (ioTCB Thyme.getCurrentTime)

-- | A relative of 'cprgGenerate' from crypto-random.
genRandomBytes :: (MonadThentosIO m, MonadThentosReader m) => Int -> m SBS
genRandomBytes i = do
    let f :: ChaChaDRG -> (ChaChaDRG, SBS)
        f r = case randomBytesGenerate i r of (output, r') -> (r', output)
    r <- view aStRandom
    liftLIO . ioTCB . modifyMVar r $ return . f

hashUserPass :: MonadThentosIO m => UserPass -> m (HashedSecret UserPass)
hashUserPass = liftLIO . ioTCB . TU.hashUserPass

hashServiceKey :: MonadThentosIO m => ServiceKey -> m (HashedSecret ServiceKey)
hashServiceKey = liftLIO . ioTCB . TU.hashServiceKey

makeUserFromFormData :: MonadThentosIO m => UserFormData -> m User
makeUserFromFormData = liftLIO . ioTCB . TU.makeUserFromFormData

loggerA :: MonadThentosIO m => Priority -> String -> m ()
loggerA prio = liftLIO . ioTCB . SLM.logger prio

loggerD :: (MonadThentosIO m, Show v) => v -> m ()
loggerD = loggerA DEBUG . show

logIfError :: (MonadThentosIO m, MonadError e m, Show e) => m a -> m a
logIfError = (`catchError` \e -> loggerA ERROR (show e) >> throwError e)

logSignupAttempt :: MonadThentosIO m => UserName -> UserEmail -> CaptchaAttempt -> m ()
logSignupAttempt name email captchaAttempt = do
    now <- getCurrentTime
    let signupAttempt = SignupAttempt name email captchaAttempt now
        logLine = cs . Builder.toByteString $ CsvBuilder.encodeRecord signupAttempt
        logLevel = CRITICAL -- for some reason the entries aren't written to the file at INFO
    liftLIO . ioTCB $ logM signupLogger logLevel (init logLine)

sendMail :: (MonadThentosIO m, MonadThentosReader m) => Maybe UserName -> UserEmail -> ST -> ST -> Maybe ST -> m ()
sendMail mName address subject body html = do
    config <- Tagged <$> getConfigField (Proxy :: Proxy '["smtp"])
    result <- liftLIO . ioTCB $ TS.sendMail config mName address subject body html
    case result of
        Right () -> return ()
        Left (TS.SendmailError s) -> do
            loggerA CRITICAL $ "error sending mail: " ++ s
            liftLIO . ioTCB . throwIO $ ErrorCall "error sending email"

-- | Render a Hastache template for plain-text output (none of the characters in context variables
-- will be escaped).
renderTextTemplate :: MonadThentosIO m => ST -> MuContext IO -> m LT
renderTextTemplate template context = liftLIO . ioTCB $ hastacheStr hastacheCfg template context
  where
    hastacheCfg = defaultConfigIO { muEscapeFunc = emptyEscape, muTemplateRead = const (pure Nothing) }

-- | This was made a top-level definition just to help GHC pick the `MonadIO IO` instance.
defaultConfigIO :: MuConfig IO
defaultConfigIO = defaultConfig

extendClearanceOnThentosSession :: MonadQuery e m => ThentosSessionToken -> m ()
extendClearanceOnThentosSession tok = do
    (_, session) <- U.query . T.lookupThentosSession $ tok
    U.extendClearanceOnAgent $ session ^. thSessAgent

extendClearanceOnThentosAuthCredentials :: MonadQuery e m => ThentosAuthCredentials -> m ()
extendClearanceOnThentosAuthCredentials (ThentosAuthCredentials mTok origin) = do
    mapM_ extendClearanceOnThentosSession mTok
    allow_ips <- getConfigField (Proxy :: Proxy '["allow_ips"])
    ips <- mapM (U.unsafeLiftIO . getHostAddr . cs) (fromMaybe [] allow_ips)
    when (hostAddr origin `elem` ips) $ U.extendClearanceOnPrincipals [PrivilegedIP]

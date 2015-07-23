{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Thentos.Test.Config
where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Data.Acid (AcidState)
import Data.Configifier ((:*>)((:*>)), Id(Id), Tagged(Tagged), MaybeO(JustO, NothingO), fromTagged)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (ST)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import System.Log.Logger (Priority(DEBUG))
import System.Log.Missing (Prio(Prio))

import Thentos.Types
import Thentos.Config
import Thentos.Action.Core (Ex)
import Thentos (createDefaultUser)

import Thentos.Test.Types


testConfig :: IO TestConfig
testConfig = do
    tmp <- thentosCreateTempDirectory
    let cfg = TestConfig
          { _tcfgTmp                    = tmp
          , _tcfgDbPath                 = tmp </> "test-db/"
          , _tcfgServerFullBackendPort  = 7118
          , _tcfgServerFullFrontendPort = 7119
          , _tcfgWebdriverHost          = "localhost"
          , _tcfgWebdriverPort          = 4451
          , _tcfgTraceHttp              = True
          }
    return cfg

thentosCreateTempDirectory :: IO FilePath
thentosCreateTempDirectory = do
    tmp <- fromMaybe "/tmp/" . lookup "TMP" <$> getEnvironment
    createTempDirectory tmp "_thentos_test_"

testThentosConfig :: TestConfig -> ThentosConfig
testThentosConfig tcfg = Tagged $
          Id Run
      :*> JustO (Id (fromTagged testFeConfig))
      :*> JustO (Id (fromTagged testBeConfig))
      :*> JustO (Id (fromTagged testProxyConfig))
      :*> NothingO
      :*> Id (fromTagged testSmtpConfig)
      :*> NothingO
      :*> Id (Timeout 3600)
      :*> Id (Timeout 3600)
      :*> Id (Timeout 3600)
      :*> JustO (Id 1800)
      :*> Id (fromTagged testLogConfig)
  where
    testFeConfig :: HttpConfig
    testFeConfig = Tagged $
          NothingO
      :*> Id "localhost"
      :*> Id (tcfg ^. tcfgServerFullFrontendPort)
      :*> NothingO
      :*> NothingO
      :*> NothingO

    testBeConfig :: HttpConfig
    testBeConfig = Tagged $
          NothingO
      :*> Id "localhost"
      :*> Id (tcfg ^. tcfgServerFullBackendPort)
      :*> NothingO
      :*> NothingO
      :*> NothingO

    testSmtpConfig :: SmtpConfig
    testSmtpConfig = Tagged $
          JustO (Id "Thentos")
      :*> Id "thentos@thentos.org"
      :*> Id "/bin/cat"
      :*> Id []

    testLogConfig :: LogConfig
    testLogConfig = Tagged $
          Id "./log/thentos.log"
      :*> Id (Prio DEBUG)

    testProxyConfig :: ProxyConfig
    testProxyConfig = Tagged $
          Id "someid"
      :*> Id (ProxyUri "localhost" 8001 "path")

godUid :: UserId
godUid = UserId 0

godName :: UserName
godName = "god"

godPass :: UserPass
godPass = "god"

createGod :: (db `Ex` DB) => AcidState db -> IO ()
createGod st = createDefaultUser st
    (Just . Tagged $
          Id (fromUserName godName)
      :*> Id (fromUserPass godPass)
      :*> Id (forceUserEmail "postmaster@localhost")
      :*> JustO (Id [RoleAdmin]) :: Maybe DefaultUserConfig)

-- | Force a Text to be parsed as email address, throwing an error if it fails.
forceUserEmail :: ST -> UserEmail
forceUserEmail t = fromMaybe (error $ "Invalid email address: " ++ show t) $ parseUserEmail t

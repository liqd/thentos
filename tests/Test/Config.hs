{-# LANGUAGE OverloadedStrings #-}

module Test.Config
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

import Thentos.Types
import Thentos.Config
import Thentos (createDefaultUser)

import Test.Types


testConfig :: IO TestConfig
testConfig = do
    tmp <- thentosCreateTempDirectory
    let cfg = TestConfig
          { _tcfgTmp                    = tmp
          , _tcfgDbPath                 = tmp </> "test-db/"
          , _tcfgRestPort               = 8002
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
      :*> NothingO
      :*> Id (fromTagged testSmtpConfig)
      :*> NothingO
      :*> Id (Timeout 3600)
      :*> Id (Timeout 3600)
      :*> Id (Timeout 3600)
      :*> JustO (Id 1800)
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

godUid :: UserId
godUid = UserId 0

godName :: UserName
godName = "god"

godPass :: UserPass
godPass = "god"

createGod :: AcidState DB -> IO ()
createGod st = createDefaultUser st
    (Just . Tagged $
          Id (fromUserName godName)
      :*> Id (fromUserPass godPass)
      :*> Id (forceUserEmail "postmaster@localhost")
      :*> JustO (Id [RoleAdmin]) :: Maybe DefaultUserConfig)

-- | Force a Text to be parsed as email address, throwing an error if it fails.
forceUserEmail :: ST -> UserEmail
forceUserEmail t = case parseUserEmail t of
    Just email -> email
    Nothing    -> error $ "Invalid email address: " ++ show t

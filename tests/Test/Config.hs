{-# LANGUAGE OverloadedStrings #-}

module Test.Config
where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Data.Acid (AcidState)
import Data.Configifier ((:*>)((:*>)), Id(Id), Tagged(Tagged), MaybeO(JustO, NothingO), fromTagged)
import Data.Maybe (fromMaybe)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import System.Posix.Files (createNamedPipe)

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
          , _tcfgMailFifo               = tmp </> "mailfifo"
          , _tcfgRestPort               = 8002
          , _tcfgServerFullBackendPort  = 7118
          , _tcfgServerFullFrontendPort = 7119
          , _tcfgWebdriverHost          = "localhost"
          , _tcfgWebdriverPort          = 4451
          }
    createNamedPipe (cfg ^. tcfgMailFifo) 0600
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
      :*> Id "/bin/cat"  -- FIXME: /bin/cat pollutes stdout.
      :*> Id []  -- [">", "file"] does not work.  use 'tee'?  or an alternative fifo?  how is that done in configifier?

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
      :*> Id "postmaster@localhost"
      :*> JustO (Id [RoleAdmin]) :: Maybe DefaultUserConfig)

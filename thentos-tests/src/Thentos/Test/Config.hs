{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Thentos.Test.Config
where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Data.Acid (AcidState)
import Data.Configifier ((:*>)((:*>)), configify, Id(Id), Tagged(Tagged),
                         MaybeO(JustO, NothingO), Source(YamlString), fromTagged)
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
import Thentos.Test.Utils


thentosCreateTempDirectory :: IO FilePath
thentosCreateTempDirectory = do
    tmp <- fromMaybe "/tmp/" . lookup "TMP" <$> getEnvironment
    createTempDirectory tmp "_thentos_test_"

thentosTestConfig :: IO ThentosConfig
thentosTestConfig = configify [YamlString [strLit|

command: "run"

backend:
    bind_port: 7118
    bind_host: "127.0.0.1"

frontend:
    bind_port: 7119
    bind_host: "127.0.0.1"

smtp:
    sender_name: "Thentos"
    sender_address: "thentos@thentos.org"
    sendmail_path: "/bin/cat"
    sendmail_args: []

proxy:
    service_id: "someid"
    endpoint: http://127.0.0.1:6541

default_user:
    name: "god"
    password: "god"
    email: "postmaster@localhost"
    roles: ["roleAdmin", "roleUser", "roleServiceAdmin", "roleUserAdmin"]

user_reg_expiration: "1800"
pw_reset_expiration: "1800"
email_change_expiration: "1800"
gc_interval: 1800

log:
    path: ./log/thentos.log
    level: DEBUG
|]]


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

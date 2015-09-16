{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Thentos.Test.Config
where

import Data.Configifier ((:*>)((:*>)), Id(Id), Tagged(Tagged), MaybeO(JustO))
import Data.Maybe (fromMaybe)
import Data.String.Conversions (ST)
import Database.PostgreSQL.Simple (Connection)

import Thentos.Types
import Thentos.Config
import Thentos (createDefaultUser)

import Thentos.Test.Utils


thentosTestConfig :: ThentosConfig
thentosTestConfig = [cfgify|

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
    sendmail_args: ["-t"]

proxy:
    service_id: "someid"
    endpoint: http://127.0.0.1:8001/path

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

database:
    name: unused
|]


godUid :: UserId
godUid = UserId 0

godName :: UserName
godName = "god"

godPass :: UserPass
godPass = "god"

createGod :: Connection -> IO ()
createGod conn = createDefaultUser conn
    (Just . Tagged $
          Id (fromUserName godName)
      :*> Id (fromUserPass godPass)
      :*> Id (forceUserEmail "postmaster@localhost")
      :*> JustO (Id [RoleAdmin]) :: Maybe DefaultUserConfig)

-- | Force a Text to be parsed as email address, throwing an error if it fails.
forceUserEmail :: ST -> UserEmail
forceUserEmail t = fromMaybe (error $ "Invalid email address: " ++ show t) $ parseUserEmail t

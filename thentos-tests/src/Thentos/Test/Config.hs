{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Thentos.Test.Config
where

import Data.Configifier ((:*>)((:*>)), Id(Id), Tagged(Tagged), MaybeO(JustO), cfgify)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (ST)
import Database.PostgreSQL.Simple (Connection)

import Thentos.Types
import Thentos.Config
import Thentos (createDefaultUser)


thentosTestConfig :: ThentosConfig
thentosTestConfig = [cfgify|

command: "run"

backend:
    bind_port: 7118
    bind_host: "127.0.0.1"
    expose_port: 7118
    expose_host: "127.0.0.1"

frontend:
    bind_port: 7119
    bind_host: "127.0.0.1"
    expose_port: 7119
    expose_host: "127.0.0.1"

purescript: ../thentos-purescript/static

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

user_reg_expiration: 30m
pw_reset_expiration: 30m
email_change_expiration: 30m
captcha_expiration: 30m
gc_interval: 30m

log:
    path: ./log/thentos.log
    level: DEBUG

database:
    name: unused

mail:
    account_verification:
        subject: "Thentos: Aktivierung Ihres Nutzerkontos"
        body: |
            Hallo {{user_name}},

            bitte nutzen Sie den folgenden Link um das Nutzerkonto zu aktivieren.

            {{activation_url}}
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

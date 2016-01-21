{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Thentos.Test.Config
    ( thentosTestConfig
    , thentosTestConfig'
    , thentosTestConfigSources
    , thentosTestConfigYaml
    , godUid
    , godName
    , godPass
    , createGod
    , forceUserEmail
    )
where

import Control.Concurrent.MVar (MVar, readMVar, newMVar)
import Database.PostgreSQL.Simple (Connection)
import Data.Configifier
    ( (:*>)((:*>)), Id(Id), Tagged(Tagged), MaybeO(JustO)
    , Source(YamlString, ShellEnv, CommandLine)
    )
import Data.Maybe (fromMaybe)
import Data.String.Conversions (ST, cs)
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import System.Environment (getEnvironment, getArgs)
import System.IO.Unsafe (unsafePerformIO)

import Thentos.Config
import Thentos (createDefaultUser)
import Thentos.Types


-- | The test suite calls `getConfig` many times, which in turn changes to the `root_path`.  If the
-- latter is relative, this won't work.  So every time we construct 'thentosTestConfig', we want to
-- change to the current directory of the last time it was called.  This function does that.
memoizeCurrentDirectory :: IO ()
memoizeCurrentDirectory = readMVar memoizeCurrentDirectoryState >>= setCurrentDirectory

{-# NOINLINE memoizeCurrentDirectoryState #-}
memoizeCurrentDirectoryState :: MVar FilePath
memoizeCurrentDirectoryState = unsafePerformIO $ getCurrentDirectory >>= newMVar


thentosTestConfig :: IO ThentosConfig
thentosTestConfig = thentosTestConfig' []

thentosTestConfig' :: [Source] -> IO ThentosConfig
thentosTestConfig' extra =
    memoizeCurrentDirectory >>
    thentosTestConfigSources >>=
    getConfigWithSources . (++ extra)

thentosTestConfigSources :: IO [Source]
thentosTestConfigSources = do
    e <- getEnvironment
    a <- getArgs
    return [thentosTestConfigYaml, ShellEnv e, CommandLine a]

thentosTestConfigYaml :: Source
thentosTestConfigYaml = YamlString . cs . unlines $
    "root_path: ../thentos-core/" :
    "" :
    "backend:" :
    "    bind_port: 7118" :
    "    bind_host: \"127.0.0.1\"" :
    "    expose_port: 7118" :
    "    expose_host: \"127.0.0.1\"" :
    "" :
    "frontend:" :
    "    bind_port: 7119" :
    "    bind_host: \"127.0.0.1\"" :
    "    expose_port: 7119" :
    "    expose_host: \"127.0.0.1\"" :
    "" :
    "allow_ips: [\"0.0.0.0\"]" :
    "smtp:" :
    "    sender_name: \"Thentos\"" :
    "    sender_address: \"thentos@thentos.org\"" :
    "    sendmail_path: \"/bin/cat\"" :
    "    sendmail_args: [\"-t\"]" :
    "" :
    "proxy:" :
    "    service_id: \"someid\"" :
    "    endpoint: http://127.0.0.1:8001/path" :
    "" :
    "default_user:" :
    "    name: \"god\"" :
    "    password: \"god\"" :
    "    email: \"postmaster@localhost\"" :
    "    roles: [\"roleAdmin\", \"roleUser\", \"roleServiceAdmin\", \"roleUserAdmin\"]" :
    "" :
    "user_reg_expiration: 30m" :
    "pw_reset_expiration: 30m" :
    "email_change_expiration: 30m" :
    "gc_interval: 30m" :
    "captcha_expiration: 30m" :
    "" :
    "log:" :
    "    path: /dev/null" :
    "    level: EMERGENCY" :
    "    stdout: False" :
        -- (change this to your like during debugging)
    "" :
    "database:" :
    "    name: thentos_tests" :
    "" :
    "email_templates:" :
    "    account_verification:" :
    "        subject: \"Thentos: Aktivierung Ihres Nutzerkontos\"" :
    "        body: |" :
    "            Hallo {{user_name}}," :
    "" :
    "            bitte nutzen Sie den folgenden Link um das Nutzerkonto zu aktivieren." :
    "" :
    "            {{activation_url}}" :
    "    user_exists:" :
    "        subject: \"Thentos: Attempted Signup\"" :
    "        body: \"Someone tried to sign up to Thentos with your email address.\"" :
    "    password_reset:" :
    "        subject: \"Thentos: Reset Password\"" :
    "        body: |" :
    "            Dear {{user_name}}," :
    "" :
    "            please use the link below to reset your password." :
    "" :
    "            {{reset_url}}" :
    []

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

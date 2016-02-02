{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Thentos.Test.Config
    ( thentosTestConfig
    , thentosTestConfig'
    , thentosTestConfigSources
    , thentosTestConfigYaml
    , forceUserEmail
    , getDefaultUser
    , getDefaultUser'
    )
where

import Control.Concurrent.MVar (MVar, readMVar, newMVar)
import Data.Configifier (Source(YamlString), (>>.), defaultSources')
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs)
import Database.PostgreSQL.Simple (Connection)
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import System.IO.Unsafe (unsafePerformIO)

import Thentos.Config hiding (getDefaultUser)
import Thentos.Transaction.Core (runThentosQuery)
import Thentos.Transaction (lookupConfirmedUserByName)
import Thentos.Types


-- | The test suite calls `readConfig` many times, which in turn changes to the `root_path`.  If the
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
    readConfigWithSources . (++ extra)

thentosTestConfigSources :: IO [Source]
thentosTestConfigSources = (thentosTestConfigYaml:) <$> defaultSources' "THENTOS_" []

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
    "    sendmail_path: \"cat\"" :
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
    "    groups: [\"groupAdmin\", \"groupUser\", \"groupServiceAdmin\", \"groupUserAdmin\"]" :
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
    "csrf_secret: 1daf3741e8a9ae1b39fd7e9cc7bab44ee31b6c3119ab5c3b05ac33cbb543289c" :
    []

-- | Force a Text to be parsed as email address, throwing an error if it fails.
--
-- FIXME: rename to @mkUserEmail@ or @mkUserEmailFailing@.  (Motivation: `force` is something that
-- takes a lazy thunk and returns a reduced value (e.g. in WHNF).  `unsafe` is something with wobbly
-- or incomplete semantics.  I think we sometimes use `mk` for smart constructors that work like ADT
-- constructors, but carry more logic.  `mk` should preferably be total, therefore the proposed
-- alternative @mkUserEmailFailing@.)  See also: `Text.Email.Validate.unsafeEmailAddress`.
forceUserEmail :: ST -> UserEmail
forceUserEmail t = fromMaybe (error $ "Invalid email address: " ++ show t) $ parseUserEmail t

-- | Return details of the default user for testing.  Fails if default user has not been created.
getDefaultUser :: ThentosConfig -> Pool Connection -> IO (UserId, UserPass, UserName)
getDefaultUser cfg conn = do
    let (upass, uname) = getDefaultUser' cfg
    Right (uid, _) <- runThentosQuery conn $ lookupConfirmedUserByName uname
    return (uid, upass, uname)

getDefaultUser' :: ThentosConfig -> (UserPass, UserName)
getDefaultUser' cfg = (upass, uname)
  where
    Just uname = UserName <$> cfg >>. (Proxy :: Proxy '["default_user", "name"])
    Just upass = UserPass <$> cfg >>. (Proxy :: Proxy '["default_user", "password"])

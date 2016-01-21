{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

module Thentos
    ( main
    , makeMain
    , makeActionState
    , createConnPoolAndInitDb
    , createDefaultUser
    , runGcLoop
    , autocreateMissingServices
    ) where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Concurrent (ThreadId, threadDelay, forkIO)
import Control.Exception (finally)
import Control.Monad (void, when, forever)
import "cryptonite" Crypto.Random (ChaChaDRG, drgNew)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Either (isRight)
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Foldable (forM_)
import Data.Pool (Pool, createPool, withResource)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs)
import Data.Void (Void)
import LIO.DCLabel (toCNF)
import System.Log.Logger (Priority(DEBUG, INFO, ERROR), removeAllHandlers)
import Text.Show.Pretty (ppShow)

import qualified Data.Map as Map

import System.Log.Missing (logger, announceAction)
import Thentos.Action
import Thentos.Action.Core (runActionWithPrivs)
import Thentos.Action.Types (Action, ActionState(..))
import Thentos.Config
import Thentos.Frontend (runFrontend)
import Thentos.Smtp (checkSendmail)
import Thentos.Transaction.Core (createDB, runThentosQuery, ThentosQuery)
import Thentos.Types
import Thentos.Util

import qualified Thentos.Backend.Api.Simple as Simple
import qualified Thentos.Transaction as T


-- * main

main :: IO ()
main = makeMain $ \ actionState mBeConfig mFeConfig -> do
    let backend = mapM_ (`Simple.runApi` actionState) mBeConfig
    let frontend = mapM_ (`runFrontend` actionState) mFeConfig

    void $ concurrently backend frontend


-- * main with abstract commands

makeMain :: (ActionState -> Maybe HttpConfig -> Maybe HttpConfig -> IO ()) -> IO ()
makeMain commandSwitch =
  do
    config :: ThentosConfig <- getConfig "devel.config"
    connPool <- createConnPoolAndInitDb config

    actionState <- makeActionState config connPool
    checkSendmail . Tagged $ config >>. (Proxy :: Proxy '["smtp"])

    _ <- runGcLoop actionState $ config >>. (Proxy :: Proxy '["gc_interval"])
    createDefaultUser connPool (Tagged <$> config >>. (Proxy :: Proxy '["default_user"]))
    _ <- runActionWithPrivs [toCNF RoleAdmin] () actionState
        (autocreateMissingServices config :: Action Void () ())

    let mBeConfig :: Maybe HttpConfig
        mBeConfig = Tagged <$> config >>. (Proxy :: Proxy '["backend"])

        mFeConfig :: Maybe HttpConfig
        mFeConfig = Tagged <$> config >>. (Proxy :: Proxy '["frontend"])

    logger INFO "Press ^C to abort."
    let run = do
            commandSwitch actionState mBeConfig mFeConfig
        finalize = do
            announceAction "shutting down hslogger" $
                removeAllHandlers

    run `finally` finalize


-- * helpers

-- | Initialise ActionState
makeActionState :: ThentosConfig -> Pool Connection -> IO ActionState
makeActionState config connPool = do
    rng :: MVar ChaChaDRG <- drgNew >>= newMVar
    return $ ActionState config rng connPool

-- | Garbage collect DB type.  (In this module because 'Thentos.Util' doesn't have 'Thentos.Action'
-- yet.  It takes the time interval in such a weird type so that it's easier to call with the
-- config.  This function should move and change in the future.)
runGcLoop :: ActionState -> Maybe Timeout -> IO ThreadId
runGcLoop _           Nothing         = forkIO $ return ()
runGcLoop actionState (Just interval) = forkIO . forever $ do
    _ <- runActionWithPrivs [toCNF RoleAdmin] () actionState (collectGarbage :: Action Void () ())
    threadDelay $ toMilliseconds interval * 1000

-- | Create a connection pool and initialize the DB by creating all tables, indexes etc. if the DB
-- is empty. Tables already existing in the DB won't be touched. The DB itself must already exist.
createConnPoolAndInitDb :: ThentosConfig -> IO (Pool Connection)
createConnPoolAndInitDb cfg = do
    connPool <- createPool createConn close
                           1    -- # of stripes (sub-pools)
                           60   -- close unused connections after .. secs
                           100  -- max number of active connections
    withResource connPool createDB
    return connPool
  where
    dbName = cfg >>. (Proxy :: Proxy '["database", "name"])
    createConn = connectPostgreSQL $ "dbname=" <> cs dbName

-- | If default user is 'Nothing' or user with 'UserId 0' exists, do
-- nothing.  Otherwise, create default user.
createDefaultUser :: Pool Connection -> Maybe DefaultUserConfig -> IO ()
createDefaultUser conn = mapM_ $ \(getDefaultUser -> (userData, roles)) -> do
    eq <- runThentosQuery conn $ (void $ T.lookupConfirmedUser (UserId 0) :: ThentosQuery Void ())
    case eq of
        Right _         -> logger DEBUG $ "default user already exists"
        Left NoSuchUser -> do
            -- user
            user <- makeUserFromFormData userData
            logger DEBUG $ "No users.  Creating default user: " ++ ppShow (UserId 0, user)
            (eu :: Either (ThentosError Void) UserId) <- runThentosQuery conn $ T.addUserPrim
                    (Just $ UserId 0) user True

            if eu == Right (UserId 0)
                then logger DEBUG $ "[ok]"
                else logger ERROR $ "failed to create default user: " ++ ppShow (UserId 0, eu, user)

            -- roles
            logger DEBUG $ "Adding default user to roles: " ++ ppShow roles
            (result :: [Either (ThentosError Void) ()]) <-
                 mapM (runThentosQuery conn . T.assignRole (UserA . UserId $ 0)) roles

            if all isRight result
                then logger DEBUG $ "[ok]"
                else logger ERROR $ "failed to assign default user to roles: " ++ ppShow (UserId 0, result, user, roles)
        Left e          -> logger ERROR $ "error looking up default user: " ++ show e

-- | Autocreate any services that are listed in the config but don't exist in the DB.
-- Dies with an error if the default "proxy" service ID is repeated in the "proxies" section.
autocreateMissingServices :: ThentosConfig -> Action Void s ()
autocreateMissingServices cfg = do
    dieOnDuplicates
    mapM_ (autocreateServiceIfMissing agent) allSids
  where
    dieOnDuplicates  =
        forM_ mDefaultProxySid $ \sid ->
            when (sid `elem` proxySids) . error $ show sid ++ " mentioned twice in config"
    allSids          = maybeToList mDefaultProxySid ++ proxySids
    mDefaultProxySid = ServiceId <$> cfg >>. (Proxy :: Proxy '["proxy", "service_id"])
    proxySids        = Map.keys $ getProxyConfigMap cfg
    agent            = UserId 0

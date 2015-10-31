{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Thentos.Frontend where

import Control.Monad.IO.Class
import Data.String.Conversions
import GHC.TypeLits (Symbol)
import Servant hiding (serveDirectory)
import Network.Wai
import Servant.Server.Internal.Router
import Servant.Server.Internal.RoutingApplication
import System.Log.Logger
import System.IO (stderr)

import qualified Servant.Utils.StaticFiles as StaticFiles
import qualified Text.Blaze.Html5 as H
import qualified Data.ByteString.Lazy.Char8 as LBS

import Servant.Missing
import System.Log.Missing
import Thentos.Action.Core
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Frontend.Handlers
import Thentos.Frontend.Handlers.Combinators
import Thentos.Frontend.Pages
import Thentos.Frontend.State
import Thentos.Frontend.TH
import Thentos.Frontend.Types
import Thentos.Types

-- FIXME: imported for testing only >>>

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Concurrent (ThreadId, threadDelay, forkIO)
import Control.Exception (finally)
import Control.Monad (void, when, forever)
import "cryptonite" Crypto.Random (ChaChaDRG, drgNew)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Either (isRight)
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Pool (Pool, createPool, withResource)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (SBS, cs)
import Data.Void (Void)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close)
import System.Log.Logger (Priority(DEBUG, INFO, ERROR), removeAllHandlers)
import Text.Show.Pretty (ppShow)
import Thentos.Transaction.Core (createDB, runThentosQuery, ThentosQuery)
import Language.Haskell.TH (Q, Exp, runIO)
import Language.Haskell.TH.Quote (dataToExpQ)
import qualified Network.HTTP.Media               as M

import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler.Simple (formatter, fileHandler, streamHandler)
import System.Log.Logger (Priority(DEBUG, CRITICAL), removeAllHandlers, updateGlobalLogger,
                          setLevel, setHandlers)
import System.Log.Missing (loggerName, logger, Prio(..))

-- FIXME: <<< imported for testing only


-- * driver

runFrontend :: HttpConfig -> ActionState -> IO ()
runFrontend config aState = do
    logger INFO $ "running frontend on " ++ show (bindUrl config) ++ "."
    serveFAction (Proxy :: Proxy FrontendH) frontendH aState >>= runWarpWithCfg config

type FrontendH =
       Get '[HTM] H.Html
  :<|> "user" :> UserH
  :<|> "dashboard" :> Get '[HTM] H.Html
  :<|> StaticContent

frontendH :: ServerT FrontendH FAction
frontendH =
       redirect' "/dashboard"
  :<|> userH
  :<|> renderDashboard DashboardTabDetails userDisplayPagelet
  :<|> staticContent


-- * static content

-- | Instead of ServeDirectory, we bake all static content into the executable.  This helps to
-- minimize the number of moving parts in the deployment.
type StaticContent =
       "screen.css" :> Get '[TextCss] LBS

staticContent :: forall m. Applicative m => ServerT StaticContent m
staticContent =
       l $(loadStaticContent "screen.css")
  where
    l = pure . LBS.pack


-- * /user

type UserH =
       UserRegisterH
  :<|> UserRegisterConfirmH
  :<|> UserLoginH

userH :: ServerT UserH FAction
userH =
       userRegisterH
  :<|> userRegisterConfirmH
  :<|> userLoginH



----------------------------------------------------------------------
-- FIXME: the rest here is testing...

-- | Create a connection pool and initialize the DB by creating all tables, indexes etc. if the DB
-- is empty. Tables already existing in the DB won't be touched. The DB itself must already exist.
createConnPoolAndInitDb :: SBS -> IO (Pool Connection)
createConnPoolAndInitDb dbName = do
    connPool <- createPool createConn close
                           1    -- # of stripes (sub-pools)
                           60   -- close unused connections after .. secs
                           100  -- max number of active connections
    withResource connPool createDB
    return connPool
  where
    createConn = connectPostgreSQL $ "dbname=" <> dbName


main :: IO ()
main = do
    config :: ThentosConfig <- getConfig "/home/mf/thentos/thentos-core/devel2.config"
    rng :: MVar ChaChaDRG   <- drgNew >>= newMVar
    let dbName = config >>. (Proxy :: Proxy '["database", "name"])
    connPool <- createConnPoolAndInitDb $ cs dbName
    let actionState = ActionState (connPool, rng, config)
        Just (feConfig :: HttpConfig) = Tagged <$> config >>. (Proxy :: Proxy '["frontend"])

    do -- logger
      let loglevel = DEBUG
      removeAllHandlers
      let fmt = simpleLogFormatter "$utcTime *$prio* [$pid][$tid] -- $msg"
      sHandler <- (\ h -> h { formatter = fmt }) <$> streamHandler stderr loglevel
      updateGlobalLogger loggerName $
        System.Log.Logger.setLevel loglevel . setHandlers [sHandler]

    runFrontend feConfig actionState

    logger INFO "Press ^C to abort."





{- old module:

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Thentos.Frontend where

import Data.ByteString (ByteString)
import Data.Configifier ((>>.))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs)
import Snap.Core (ifTop, redirect')
import Snap.Http.Server (defaultConfig, setBind, setPort, setVerbose)
import Snap.Snaplet.Session.Backends.CookieSession (initCookieSessionManager)
import Snap.Snaplet (SnapletInit, makeSnaplet, nestSnaplet, addRoutes, wrapSite)
import Snap.Util.FileServe (serveDirectory)
import System.Log.Missing (logger)
import System.Log (Priority(INFO))

import qualified Thentos.Frontend.Handlers as H
import qualified Thentos.Frontend.Pages as P

import Snap.Missing (serveSnaplet)
import Thentos.Action.Core
import Thentos.Config
import Thentos.Frontend.Handlers.Combinators
import Thentos.Frontend.Types


runFrontend :: HttpConfig -> ActionState -> IO ()
runFrontend config asg = do
    logger INFO $ "running frontend on " <> show (bindUrl config) <> "."
    serveSnaplet snapConfig (frontendApp asg config)
  where
    host :: ByteString = cs $ config >>. (Proxy :: Proxy '["bind_host"])
    port :: Int = config >>. (Proxy :: Proxy '["bind_port"])
    snapConfig = setPort port
               . setBind host
               . setVerbose False
               $ defaultConfig

frontendApp :: ActionState -> HttpConfig -> SnapletInit FrontendApp FrontendApp
frontendApp (ActionState (st, rn, _cfg)) feConf =
    makeSnaplet "Thentos" "The Thentos universal user management system" Nothing $ do
        wrapSite H.disableCaching
        wrapSite csrfify
        addRoutes routes
        FrontendApp st rn _cfg <$>
            nestSnaplet "sess" sess
               (initCookieSessionManager "site_key.txt" "sess" (Just 3600)) <*>
            pure feConf

routes :: [(ByteString, FH ())]
routes = [ -- default entry point
           ("", ifTop $ redirect' "/dashboard" 303)

           -- if not logged in
         , ("user/register", H.userRegister)
         , ("user/register_confirm", H.userRegisterConfirm)
         , ("user/login", H.userLogin)
         , ("user/reset_password_request", H.resetPassword)
         , ("user/reset_password", H.resetPasswordConfirm)

           -- if logged in
         , ("user/logout", H.userLogout)
         , ("user/update_email", H.emailUpdate)
         , ("user/update_email_confirm", H.emailUpdateConfirm)
         , ("user/update_password", H.passwordUpdate)

         , ("service/register", H.serviceRegister)
         , ("service/login", H.serviceLogin)

         , ("/dashboard", redirect' "/dashboard/details" 303)
             -- (this could also look up the last page the user was
             -- on, and navigate there.)

             -- (dashboard should probably be a snaplet.  if only for
             -- the routing table and the call to the
             -- dashboardPagelet.)

         , ("/dashboard/details",     renderDashboard P.DashboardTabDetails P.userDisplayPagelet)
         , ("/dashboard/services",    renderDashboard P.DashboardTabServices $ P.userServicesDisplayPagelet)
         , ("/dashboard/ownservices", H.serviceCreate)
         , ("/dashboard/users",       renderDashboard P.DashboardTabUsers $ \ _ _ -> "nothing here yet!")

         -- static files
         , ("", serveDirectory "snap/static")

         -- 404 error page if no other route exists
         , ("", H.unknownPath)
         ]

-}

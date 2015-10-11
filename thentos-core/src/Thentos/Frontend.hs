{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeOperators          #-}

module Thentos.Frontend where

import Data.ByteString (ByteString)
import Data.Configifier ((>>.))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs)
import System.Log.Missing (logger)
import System.Log (Priority(INFO))
import Network.Wai
import Servant.Server
import Servant
import Servant.HTML.Blaze
import Data.Typeable
import Servant.API ((:>))
import Servant.API.ContentTypes (AllCTRender, AllMimeRender, allMime, IsNonEmpty)
import Servant.Server (HasServer, ServerT, ServantErr, route, (:~>)(Nat))
import Servant.Server.Internal (methodRouter, Router(..), RouteResult(Route, FailFatal))
import Servant.Server.Internal.RoutingApplication (addMethodCheck)
import Servant.Server.Internal.ServantErr (err400, err401, err403, err404, err500, errBody, errHeaders)
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Data.ByteString.Builder (toLazyByteString)
import URI.ByteString
import Control.Monad.State
import Control.Lens ((^.))

import Thentos.Action.Core
import Thentos.Config
import Thentos.Types
import Thentos.Util (fmapLM)
import Thentos.Frontend.Types
import Thentos.Frontend.Session
import Thentos.Backend.Core

-- import Thentos.Frontend.Handlers.Combinators
-- import qualified Thentos.Frontend.Handlers as H
-- import qualified Thentos.Frontend.Pages as P


runApi :: HttpConfig -> ActionState -> IO ()
runApi cfg actionState = do
    logger INFO $ "running rest api Thentos.Frontend on " ++ show (bindUrl cfg) ++ "."
    runWarpWithCfg cfg $ serveApi actionState

serveApi :: ActionState -> Application
serveApi = addCacheControlHeaders . serve (Proxy :: Proxy FrontendApi) . api

api :: ActionState -> Server FrontendApi
api actionState = enter t . runFrontendApi
  where
    t :: Action FrontendError :~> ExceptT ServantErr IO
    t = enterAction actionState frontendActionErrorToServantErr Nothing


type FrontendApi = Header "cookie" FrontendSessionData :> FApi

runFrontendApi :: Maybe FrontendSessionData -> ServerT FApi (Action FrontendError)
runFrontendApi mSessionState = enter t runFApi
  where
    t :: FrontendAction :~> Action FrontendError
    t = Nat $ \(FrontendAction (StateT a)) -> fst <$> a sessionState

    sessionState :: SessionPayload FrontendSessionData
    sessionState = case mSessionState of
      Just p -> SessionPayload p
      Nothing -> SessionNothing


type FApi = "bool" :> Get '[HTML] Bool :<|> "int" :> Get '[HTML] Int

runFApi :: ServerT FApi FrontendAction
runFApi = b :<|> i
  where
    b = do
          -- sessionData <- get
          return True -- $ case sessionData ^. fsdLogin of Just _ -> True; Nothing -> False
    i = do
          -- sessionData <- get
          return 3 -- $ case sessionData ^. fsdLogin of Just _ -> 1; Nothing -> 2



frontendErrorInfo :: FrontendError -> ErrorInfo ST
frontendErrorInfo = t
  where
    t (FrontendErrorRedirectRR rr) = tUri . serializeRelativeRef $ rr
    t (FrontendErrorRedirectURI uri) = tUri . serializeURI $ uri

    tUri builder =
      ( Nothing
      , err303 { errHeaders = [("Location", cs . toLazyByteString $ builder)] }
      , "(redirect)"
      )

frontendActionErrorToServantErr :: ActionError FrontendError -> IO ServantErr
frontendActionErrorToServantErr = errorInfoToServantErr mkServantErr .
                                     actionErrorInfo (thentosErrorInfo frontendErrorInfo)




{-
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

frontendApp :: ActionState -> HttpConfig -> SnapletInit FrontendActionState FrontendActionState
frontendApp (ActionState (st, rn, _cfg)) feConf =
    makeSnaplet "Thentos" "The Thentos universal user management system" Nothing $ do
        wrapSite H.disableCaching
        wrapSite csrfify
        addRoutes routes
        FrontendActionState st rn _cfg <$>
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

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ViewPatterns           #-}

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
import Servant.Server.Internal.Enter

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


type FrontendApi = Header "Cookie" FrontendSessionData :> FApi With

runFrontendApi :: Maybe FrontendSessionData -> ServerT (FApi With) (Action FrontendError)
runFrontendApi mp = enter' (maybe SessionNothing SessionPayload mp) runFApi



--------------------------------------------------------------------------

data With
data Without

type family H w a :: * where
  H With    a = Headers '[Header "Set-Cookie" FrontendSessionData] a
  H Without a = a


class Enter' typ arg where
    type TransEnter' typ arg
    enter' :: arg -> typ -> TransEnter' typ arg

instance (Enter' typ1 arg1, Enter' typ2 arg1) => Enter' (typ1 :<|> typ2) arg1 where
    type TransEnter' (typ1 :<|> typ2) arg1 = TransEnter' typ1 arg1 :<|> TransEnter' typ2 arg1
    enter' e (a :<|> b) = enter' e a :<|> enter' e b

instance (Enter' b arg) => Enter' (a -> b) arg where
    type TransEnter' (a -> b) arg = a -> TransEnter' b arg
    enter' arg f a = enter' arg (f a)

instance Enter' (ServerT (FApi Without) FrontendAction) (SessionPayload FrontendSessionData) where
    type TransEnter' (ServerT (FApi Without) FrontendAction)
            (SessionPayload FrontendSessionData) =
                ServerT (FApi With) (Action FrontendError)
    enter' s (FrontendAction (StateT m)) = do
        (v, s') <- m s
        return $ addHeader s' v

--------------------------------------------------------------------------



type FApi a =
       "bool" :> Get '[HTML] (H a Bool)
  :<|> "int" :> Get '[HTML] (H a Int)

runFApi :: ServerT (FApi Without) FrontendAction
runFApi = do
    -- SessionPayload sessionData <- get
    b :<|> i
  where
    b = do
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

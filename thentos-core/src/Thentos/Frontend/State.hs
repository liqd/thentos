{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Thentos.Frontend.State
    ( serveFActionStack
    , serveFAction
    , enterFAction
    , getFrontendCfg
    , fActionServantErr
    )
where

import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Data.Configifier (Tagged(Tagged))
import LIO.TCB (ioTCB)
import Network.Wai (Application)
import Servant (ServantErr, HasServer, ServerT, Server, (:~>)(Nat))
import Servant.Server (errHTTPCode, errHeaders, errBody, err303, err404, err400, err500)
import Servant.Utils.Enter (Enter)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Web.Cookie (SetCookie, def, setCookieName, setCookiePath)

import Thentos.Prelude
import Thentos.Action.Core
import Thentos.Action.Types
import Thentos.Action.TCB
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Frontend.Types
import Thentos.CookieSession

import qualified Thentos.Frontend.Pages.Core as Pages


-- BUG #406: EU-required user notifications about cookies
-- FUTUREWORK: more efficient refresh (only if changed or after 20% of the age has been passed)


-- * errors

fActionServantErr :: ActionError FActionError -> IO ServantErr
fActionServantErr = errorInfoToServantErr mkServErr .
                                    actionErrorInfo (thentosErrorInfo f)
  where
    f :: FActionError -> (Maybe (Priority, String), ServantErr, ST)
    f (FActionError303 uri) =
        (Nothing, err303 { errHeaders = [("Location", uri)] }, "redirect: " <> cs uri)
    f FActionError404 =
        (Nothing, err404, "page not found.")
    f e@FActionErrorNoToken =
        (Just (DEBUG, show e), err400, "email confirmation url broken: no token.")
    f e@FActionErrorCreateService =
        (Just (DEBUG, show e), err400, "could not create service.")
    f e@FActionErrorServiceLoginNoCbUrl =
        (Just (DEBUG, show e), err400, "no or broken callback url.")
    f e@(FActionError500 _) =
        (Just (ERROR, show e), err500, "we are very sorry.")

    mkServErr :: ServantErr -> ST -> ServantErr
    mkServErr baseErr msg = baseErr
        { errBody = cs . renderHtml $ makeErrorPage (errHTTPCode baseErr) msg
        , errHeaders = ("Content-Type", "text/html; charset=utf-8") : errHeaders baseErr
        }

    makeErrorPage :: Int -> ST -> Html
    makeErrorPage 403 = const Pages.permissionDeniedPage
    makeErrorPage 404 = const Pages.notFoundPage
    makeErrorPage _   = Pages.errorPage . cs


-- * middleware

-- We set the path to "/" to keep the browser from setting the current path for the cookie and hence
-- storing many cookies instead of only one.
--
-- FIXME: make 'SetCookie' configurable with configifier.
-- At least some configuration is possible now, see the SetCookie parameter to serveFAction.
thentosSetCookie :: SetCookie
thentosSetCookie = def { setCookieName = "thentos", setCookiePath = Just "/" }


-- * frontend action monad

type FActionStack = ActionStack FActionError FrontendSessionData

serveFActionStack :: forall api.
        ( HasServer api '[]
        , Enter (ServerT api FActionStack) (FActionStack :~> ExceptT ServantErr IO) (Server api)
        )
     => Proxy api -> ServerT api FActionStack -> ActionEnv -> IO Application
serveFActionStack proxy fServer aState =
    serveFAction proxy (Proxy :: Proxy FrontendSessionData) thentosSetCookie
                 extendClearanceOnThentosSession (Nat (liftLIO . ioTCB)) (Nat run) fServer
  where
    run :: FActionStack a -> ExceptT ServantErr IO a
    run = ExceptT . (>>= _Left fActionServantErr) . (fst <$>)
        . runActionE emptyFrontendSessionData aState

getFrontendCfg :: MonadThentosConfig e m => m HttpConfig
getFrontendCfg = do
    Just (feConfig :: HttpConfig) <- (Tagged <$>) <$> getConfigField (Proxy :: Proxy '["frontend"])
    return feConfig

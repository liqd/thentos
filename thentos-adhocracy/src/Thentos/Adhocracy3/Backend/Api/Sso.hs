{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE LambdaCase                               #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}

-- | This is a variant of "Thentos.Backend.Api.Adhocracy3" that throws errors on the old
-- authentication end points and instead performs github authentication.  The intricacies of the
-- protocol look something like this:
--
-- >>> step req# description
-- >>>
-- >>> 01.  1.   BRO -> A3:    "can i give you access to my sso data on github so you know it's me?"
-- >>> 02.  1.   A3  -> BRO:   "ok, here is a request token."
-- >>> 03.  2.   BRO -> GIH:   "a3 gave me this request token."
-- >>> 04.  2.   GIH -> BRO:   "do you want to let a3 access your github-verified email address?"
-- >>> 05.  3.   BRO -> GIH:   "sure!"
-- >>> 06.  3.   GIH -> BRO:   "ok, please pass this access token on to a3."
-- >>> 07.  4.   BRO -> A3:    "here you go: take this to github and check out my email address"
-- >>> 08.  5.     A3  -> GIH: "here is an access token."
-- >>> 09.  5.     GIH -> A3:  "here is some accessed information: email address."
-- >>> 10.  4.   A3  -> BRO:   "here is your 'ThentosSessionToken'."
--
-- (A3 is represented by this module; the actual application first sees any traffic only after the
-- browser has received the response in step 8 and sends a new, authenticated request.)
--
-- This workflow is complicated further by the fact that rest api and traditional http/html both
-- play a role in both a3 and thentos.  So a differnt (and equally valid) view on what happens is
-- this (javascript pseudo-code, as will be delivered by thentos frontend):
--
--
-- >>> // this function is called when the login button is clicked.
-- >>> function handle_sso_login_button_click() {
-- >>>     var request_url = post(backend + "/sso/github/request", { data: {} });
-- >>>     redirect(request_url);
-- >>> }
-- >>>
-- >>> // this function is registered under the route of the auth callback url.  that is, when the browser
-- >>> // is redirected back from github, it will call this function first.
-- >>> function handle_confirm_redirect() {
-- >>>
-- >>>     // extract authentication token from route (or from the function arguments)
-- >>>     // pass to thentos via rest api
-- >>>     // extract target route and session token from login response from thentos
-- >>>     // activate session token
-- >>>     // re-route to target route (something along the lines of `/logged_in/dashboard`)
-- >>>
-- >>> }
--
-- FIXME: implement this.
--
module Thentos.Adhocracy3.Backend.Api.Sso where

import Control.Monad.Except (throwError, catchError)
import Control.Monad (mzero)
import Data.Aeson (Value(Object), ToJSON, FromJSON, (.:), (.=), object)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs)
import LIO.Core (liftLIO)
import LIO.TCB (ioTCB)
import Network.OAuth.OAuth2
    ( OAuth2Result, AccessToken(..), OAuth2(..)
    , authorizationUrl, accessTokenUrl, doJSONPostRequest, appendQueryParam, authGetJSON
    )
import Network.Wai (Application)
import Servant.API ((:<|>)((:<|>)), (:>), Capture, Post, JSON)
import Servant.Server (Server, serve, enter)
import System.Log (Priority(INFO))

import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding   as ST
import qualified Network.HTTP.Client  as Client
import qualified Network.HTTP.Conduit as Http

import System.Log.Missing
import Thentos.Adhocracy3.Backend.Core
import Thentos.Adhocracy3.Types
import Thentos.Backend.Api.Proxy (ServiceProxy, serviceProxy)
import Thentos.Backend.Core
import Thentos.Config

import qualified Thentos.Action as A
import qualified Thentos.Action.Core as AC
import qualified Thentos.Adhocracy3.Backend.Api.Simple as A3


-- * main

runBackend :: HttpConfig -> AC.ActionState -> IO ()
runBackend cfg asg = do
    logger INFO $ "running rest api (a3 style with SSO) on " ++ show (bindUrl cfg) ++ "."
    manager <- Client.newManager Client.defaultManagerSettings
    runWarpWithCfg cfg $ serveApi manager asg

serveApi :: Client.Manager -> AC.ActionState -> Application
serveApi manager = addCorsHeaders A3.a3corsPolicy . addCacheControlHeaders . serve (Proxy :: Proxy Api) .
           api manager


-- * api

type ThentosSso =
       "sso" :> "github" :> ThentosSsoGithub

type ThentosSsoGithub =
       "request" :> Capture "currenturl" URL :> Post '[JSON] AuthRequest
  :<|> "confirm" :> Capture "state" ST :> Capture "code" ST :> Capture "redirectback" URL
          :> Post '[] RedirectAndSetCookie

data RedirectAndSetCookie = RedirectAndSetCookie
    { redirectUri :: URI
    , cookie :: Cookie
    }

thentosSso :: AC.ActionState -> Server ThentosSso
thentosSso actionState = enter (enterAction actionState a3ActionErrorToServantErr Nothing) $
       githubRequest
  :<|> githubConfirm

-- | Like 'A3.thentosApi', but responds with 404 on all end points rather than handling
-- them as in "Thentos.Backend.Api.Adhocracy3".  This is to make sure the proxy handler
-- won't let any user management requests through that have been sent by confused clients.
thentosApi404 :: AC.ActionState -> Server A3.ThentosApi
thentosApi404 actionState = enter (enterAction actionState a3ActionErrorToServantErr Nothing) $
       addUser
  :<|> activate
  :<|> login
  :<|> login
  :<|> resetPassword

api :: Client.Manager -> AC.ActionState -> Server Api
api manager actionState =
       thentosA3Sso  actionState
  :<|> thentosApi404 actionState
  :<|> serviceProxy manager A3.a3ProxyAdapter actionState


-- * handler

addUser :: A3.A3UserWithPass -> A3Action A3.TypedPathWithCacheControl
addUser _ = error "404"  -- FIXME: respond with a non-internal error

activate :: A3.ActivationRequest -> A3Action A3.RequestResult
activate _ = error "404"  -- FIXME: respond with a non-internal error

login :: A3.LoginRequest -> A3Action A3.RequestResult
login _ = error "404"  -- FIXME: respond with a non-internal error

resetPassword :: A3.PasswordResetRequest -> A3Action A3.RequestResult
resetPassword _ = error "404"  -- FIXME: respond with a non-internal error


data AuthRequest = AuthRequest ST

instance ToJSON AuthRequest where
    toJSON (AuthRequest st) = object ["redirect" .= st]


data GithubUser = GithubUser { gid   :: GithubId
                             , gname :: ST
                             , gEmail :: UserEmail
                             } deriving (Show, Eq)

instance FromJSON GithubUser where
    parseJSON (Object o) = do
        email <- o .: "email"
        case parseUserEmail email of
            Just userMail ->
                GithubUser
                    <$> o .: "id"
                    <*> o .: "name"
                    <*> pure userMail
            Nothing -> mzero
    parseJSON _ = mzero

-- | FIXME: move this to config.  this may also be a point in favour of configifier: we now need to
-- start thinking about composing configs just like we compose apis.
--
-- https://developer.github.com/guides/basics-of-authentication/
-- https://developer.github.com/v3/oauth/
-- https://github.com/settings/applications/214371
githubKey :: OAuth2
githubKey = OAuth2 { oauthClientId = "c4c9355b9ea698f622ba"
                   , oauthClientSecret = "51009ec786aa61296ac2f2564f9b5f1fbb23a24f"
                   , oauthOAuthorizeEndpoint = "https://github.com/login/oauth/authorize"
                   , oauthAccessTokenEndpoint = "https://github.com/login/oauth/access_token"
                   }

setCallback :: URL -> OAuth2 -> OAuth2
setCallback redirectback o = o { oauthCallback = Just $ "https://thentos-dev-frontend.liqd.net/sso/github/confirm?redirectback=" <> redirectback }

-- | FIXME: document!
githubRequest :: URL -> A3Action AuthRequest
githubRequest currenturl = do
    state <- A.addNewSsoToken
    return . AuthRequest . cs $
        authorizationUrl (setCallback currenturl githubKey) `appendQueryParam` [("state", cs $ fromSsoToken state)]


-- | FIXME: document!
githubConfirm :: ST -> ST -> URL -> A3Action RedirectAndSetCookie
githubConfirm state code redirectback = do
        A.lookupAndRemoveSsoToken (SsoToken state)
        mgr <- liftLIO . ioTCB $ Http.newManager Http.tlsManagerSettings
        tok <- confirm mgr
        return $ RedirectAndSetCookie redirectback tok
  where
    confirm mgr = do
        eToken :: OAuth2Result AccessToken <- liftLIO . ioTCB $ do
            let (url, body) = accessTokenUrl githubKey $ ST.encodeUtf8 code
            doJSONPostRequest mgr githubKey url (body ++ [("state", cs state)])

        case eToken of
            Right token -> do
                eGhUser :: OAuth2Result GithubUser
                    <- liftLIO . ioTCB $ authGetJSON mgr token "https://api.github.com/user"
                case eGhUser of
                    Right ghUser -> loginGithubUser ghUser
                    Left e -> throwError $ SsoErrorCouldNotAccessUserInfo e
            Left e -> throwError $ SsoErrorCouldNotGetAccessToken e


-- | FIXME: document!
loginGithubUser :: GithubUser -> A3Action Token
loginGithubUser (GithubUser ghId uname email) = do
    let makeTok = A.startThentosSessionByGithubId ghId

    (_, tok) <- makeTok `catchError`
        \case BadCredentials -> do
                _ <- A.addGithubUser (UserName uname) ghId email
                makeTok
              e -> throwError e
    return tok

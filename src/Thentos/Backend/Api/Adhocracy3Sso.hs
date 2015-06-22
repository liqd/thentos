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
-- >>> 04.  2.   GIH -> BRO:   "do you want to let a3 access your name and t-shirt size?"
-- >>> 05.  3.   BRO -> GIH:   "sure!"
-- >>> 06.  3.   GIH -> BRO:   "ok, please pass this access token on to a3."
-- >>> 07.  4.   BRO -> A3:    "here you go: take this to github and check out my t-shirt size."
-- >>> 08.  5.     A3  -> GIH: "here is an access token."
-- >>> 09.  5.     GIH -> A3:  "here is some accessed information: t-shirt size and name."
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
-- >>>     open_browser_tab(request_url);
-- >>> }
-- >>>
-- >>> // this function is registered under the route of the auth callback url.  that is, when the browser
-- >>> // is redirected back from github, it will call this function first.
-- >>> function handle_confirm_redirect() {
-- >>>
-- >>>     // extract authentication token from request
-- >>>     // pass to thentos rest api
-- >>>     // process login response
-- >>>     // proceed normally
-- >>>
-- >>> }
--
-- FIXME: implement this.
--
module Thentos.Backend.Api.Adhocracy3Sso where

import Control.Applicative ((<$>), (<*>))
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
import qualified Network.HTTP.Conduit as Http

import System.Log.Missing
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Types
import Thentos.Backend.Api.Proxy (ServiceProxy, serviceProxy)

import qualified Thentos.Action as A
import qualified Thentos.Action.Core as AC
import qualified Thentos.Backend.Api.Adhocracy3 as A3


-- * main

runBackend :: HttpConfig -> AC.ActionState DB -> IO ()
runBackend cfg asg = do
    logger INFO $ "running rest api (a3 style with SSO) on " ++ show (bindUrl cfg) ++ "."
    runWarpWithCfg cfg $ serveApi asg

serveApi :: AC.ActionState DB -> Application
serveApi = addResponseHeaders . serve (Proxy :: Proxy Api) . api


-- * api

type ThentosA3Sso =
       "sso" :> "github" :>
            ("request" :> Post '[JSON] AuthRequest
        :<|> "confirm" :> Capture "state" ST :> Capture "code" ST :> Post '[JSON] A3.RequestResult)

type Api = ThentosA3Sso :<|> A3.ThentosApi :<|> ServiceProxy

thentosA3Sso :: AC.ActionState DB -> Server ThentosA3Sso
thentosA3Sso actionState = enter (enterAction actionState Nothing) $
       githubRequest
  :<|> githubConfirm

-- | Like 'A3.thentosApi', but responds with 404 on all end points rather than handling
-- them as in "Thentos.Backend.Api.Adhocracy3".  This is to make sure the proxy handler
-- won't let any user management requests through that have been sent by confused clients.
thentosApi404 :: AC.ActionState DB -> Server A3.ThentosApi
thentosApi404 actionState = enter (enterAction actionState Nothing) $
       addUser
  :<|> activate
  :<|> login
  :<|> login

api :: AC.ActionState DB -> Server Api
api actionState =
       thentosA3Sso  actionState
  :<|> thentosApi404 actionState
  :<|> serviceProxy  actionState


-- * handler

addUser :: A3.A3UserWithPass -> AC.Action DB (A3.A3Resource A3.A3UserNoPass)
addUser _ = error "404"  -- FIXME: respond with a non-internal error

activate :: A3.ActivationRequest -> AC.Action DB A3.RequestResult
activate _ = error "404"  -- FIXME: respond with a non-internal error

login :: A3.LoginRequest -> AC.Action DB A3.RequestResult
login _ = error "404"  -- FIXME: respond with a non-internal error


data AuthRequest = AuthRequest ST

instance ToJSON AuthRequest where
    toJSON (AuthRequest st) = object ["redirect" .= st]


data GithubUser = GithubUser { gid   :: Integer
                             , gname :: ST
                             } deriving (Show, Eq)

instance FromJSON GithubUser where
    parseJSON (Object o) = GithubUser
                           <$> o .: "id"
                           <*> o .: "name"
    parseJSON _ = mzero

-- | FIXME: move this to config.  this may also be a point in favour of configifier: we now need to
-- start thinking about composing configs just like we compose apis.
--
-- http://developer.github.com/v3/oauth/
-- https://github.com/settings/applications/214371
githubKey :: OAuth2
githubKey = OAuth2 { oauthClientId = "c4c9355b9ea698f622ba"
                   , oauthClientSecret = "51009ec786aa61296ac2f2564f9b5f1fbb23a24f"
                   , oauthCallback = Just "https://thentos-dev-frontend.liqd.net/sso/github/confirm"
                   , oauthOAuthorizeEndpoint = "https://github.com/login/oauth/authorize"
                   , oauthAccessTokenEndpoint = "https://github.com/login/oauth/access_token"
                   }


-- | FIXME: document!
githubRequest :: AC.Action DB AuthRequest
githubRequest = do
    state <- A.freshRandomName

    -- FIXME: store state in DB.

    return . AuthRequest . cs $ authorizationUrl githubKey `appendQueryParam` [("state", cs state)]


-- | FIXME: document!
githubConfirm :: ST -> ST -> AC.Action DB A3.RequestResult
githubConfirm state code = do

    -- FIXME: lookup state in DB and crash if it does not exist.  remove if it does exist.

    mgr <- liftLIO . ioTCB $ Http.newManager Http.conduitManagerSettings

    eToken :: OAuth2Result AccessToken
        <- liftLIO . ioTCB $ do
            let (url, body) = accessTokenUrl githubKey $ ST.encodeUtf8 code
            doJSONPostRequest mgr githubKey url (body ++ [("state", cs state)])

    case eToken of
        Right token  -> do
            eGhUser :: OAuth2Result GithubUser
                <- liftLIO . ioTCB $ authGetJSON mgr token "https://api.github.com/user"
            liftLIO . ioTCB $ Http.closeManager mgr  -- FIXME: use something like `finalize`

            case eGhUser of
                Right ghUser -> loginGithubUser ghUser
                Left e -> return $ A3.RequestError ["could not access github user info", cs e]
                    -- FIXME: throw all errors as exceptions, and handle them before returning from
                    -- this function.

        Left e -> do
            liftLIO . ioTCB $ Http.closeManager mgr
            return $ A3.RequestError ["could not obtain access token", cs e]


-- | FIXME: document!
loginGithubUser :: GithubUser -> AC.Action DB A3.RequestResult
loginGithubUser (GithubUser _ uname) = do
    let makeTok = A.startThentosSessionByUserName (UserName uname) (UserPass "")

    (_, tok) <- makeTok `catchError`
        \case BadCredentials -> do
                _ <- A.addUser $ UserFormData (UserName uname) (UserPass "") (UserEmail $ error "no email")
                makeTok
              e -> throwError e

    return $ A3.RequestSuccess (A3.Path "/dashboard") tok

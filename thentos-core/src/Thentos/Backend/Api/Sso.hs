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
--
-- https://developer.github.com/guides/basics-of-authentication/
-- https://developer.github.com/v3/oauth/
--
--
-- TODO:
--   - move githubKey contents to config
--   - PR to hoauth2: split up OAuth2 type into two, one containing the callback (as a mandatory
--     value, not as Maybe), and one containing everything else.  change api so that there are fewer
--     partial functions.  also, base everything on uri-bytestring.
--   - 'Thentos.Config.exposeUrl' operates on 'ST', but should operate on 'URI'.  Go through entire
--     module there and find everything that needs to be made uri-bytestring-aware as well.
--   - for github user lookup, distinguish between "bad password" and "user does not exist".  (there
--     are some interesting fringe cases where existing users want to switch to github sso the other
--     way around.  find them and fix them!)
module Thentos.Backend.Api.Sso where

import Control.Monad.Except (throwError, catchError)
import Control.Monad (mzero)
import Data.Aeson (Value(Object), ToJSON, FromJSON, (.:), (.=), object)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (SBS, ST, cs)
import Data.Typeable (Typeable)
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
import URI.ByteString
import Data.Configifier (Tagged(Tagged), (>>.))

import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding   as ST
import qualified Network.HTTP.Client  as Client
import qualified Network.HTTP.Conduit as Http

import System.Log.Missing
import Thentos.Action as A
import Thentos.Action.Core
import Thentos.Backend.Api.Proxy (ServiceProxy, serviceProxy)
import Thentos.Backend.Core
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Types

import qualified Thentos.Action.Core as AC


-- * api

type ThentosSso =
       "sso" :> "github" :> ThentosSsoGithub

-- | Thentos needs to provide two end-points: one for requesting the SSO that sends the browser off
-- to github for negotiating the authorization token, and one that the browser can come back to with
-- that authorization token from github.
type ThentosSsoGithub =
       "request" :> Capture "return_uri" URI :> Post '[JSON] AuthRequest
  :<|> "confirm" :> Capture "state" ST :> Capture "code" ST :> Capture "return_uri" URI
          :> Post '[JSON] RedirectToService

data RedirectToService = RedirectToService URI ThentosSessionToken

instance ToJSON RedirectToService where
    toJSON (RedirectToService uri tok) = object
        [ "uri" .= (Aeson.String . cs . serializeURI' $ uri)
        , "session" .= tok
        ]

data AuthRequest = AuthRequest ST

instance ToJSON AuthRequest where
    toJSON (AuthRequest st) = object ["redirect" .= st]

data GithubUser = GithubUser
    { gId    :: GithubId
    , gName  :: ST
    , gEmail :: UserEmail
    }
  deriving (Show, Eq)

instance FromJSON GithubUser where
    parseJSON (Object o) = GithubUser
        <$> o .: "id"
        <*> o .: "name"
        <*> (o .: "email" >>= maybe mzero return . parseUserEmail)
    parseJSON _ = mzero


-- * handler

thentosSso :: AC.ActionState -> Server ThentosSso
thentosSso actionState@(ActionState (_, _, cfg)) =
    enter (enterAction actionState baseActionErrorToServantErr Nothing) $
       githubRequest thentosHttp
  :<|> githubConfirm
  where
    thentosHttp :: HttpConfig
    thentosHttp = Tagged . fromMaybe (error "blargh") $ cfg >>. (Proxy :: Proxy '["backend"])

githubKey :: OAuth2
githubKey = OAuth2 { oauthClientId = "..."
                   , oauthClientSecret = "..."
                   , oauthOAuthorizeEndpoint = "https://github.com/login/oauth/authorize"
                   , oauthAccessTokenEndpoint = "https://github.com/login/oauth/access_token"
                   , oauthCallback = Nothing
                   }

-- | Thentos proxy needs to capture the redirect back from the token server, and then redirect back
-- to the service.  This makes two nested redirects.  The first is specified by a `HttpConfig`,
-- because (the path of that `URI` is fixed), and the second by a `URI`.
setCallback :: HttpConfig -> URI -> OAuth2 -> OAuth2
setCallback thentosHttp toService o = o { oauthCallback = Just $ serializeURI' toThentos }
  where
    toThentos :: URI
    toThentos = case parseURI strictURIParserOptions . cs $ exposeUrl thentosHttp of
        Right uri -> uri
            { uriPath  = "/sso/github/confirm"
            , uriQuery = Query [("redirect_to_service", serializeURI' toService)]
            }
        Left e -> error $ "setCallback: internal error: " ++ show e

-- | ...
githubRequest :: (Show e, Typeable e) => HttpConfig -> URI -> Action e AuthRequest
githubRequest thentosHttp currenturl = do
    state <- A.addNewSsoToken
    return . AuthRequest . cs $
        authorizationUrl (setCallback thentosHttp currenturl githubKey)
            `appendQueryParam` [("state", cs $ fromSsoToken state)]

-- | ...
githubConfirm :: forall e . (Show e, Typeable e) => ST -> ST -> URI -> Action e RedirectToService
githubConfirm state code redirectback = do
        A.lookupAndRemoveSsoToken (SsoToken state)
        mgr <- liftLIO . ioTCB $ Http.newManager Http.tlsManagerSettings
        tok <- confirm mgr
        return $ RedirectToService redirectback tok
  where
    confirm :: Http.Manager -> Action e ThentosSessionToken
    confirm mgr = do
        eToken :: OAuth2Result AccessToken <- liftLIO . ioTCB $ do
            let (url, body) = accessTokenUrl githubKey $ ST.encodeUtf8 code
            doJSONPostRequest mgr githubKey url (body ++ [("state", cs state)])

        case eToken of
            Right token -> do
                eGhUser :: OAuth2Result GithubUser
                    <- liftLIO . ioTCB $ authGetJSON mgr token "https://api.github.com/user"  -- FIXME: store this uri somewhere in a type.
                case eGhUser of
                    Right ghUser -> loginGithubUser ghUser
                    Left e -> throwError $ SsoErrorCouldNotAccessUserInfo e
            Left e -> throwError $ SsoErrorCouldNotGetAccessToken e

-- | ...
loginGithubUser :: (Show e, Typeable e) => GithubUser -> Action e ThentosSessionToken
loginGithubUser (GithubUser ghId uname email) = do
    let makeTok = A.startThentosSessionByGithubId ghId

    (_, tok) <- makeTok `catchError`
        \case BadCredentials -> do
                _ <- A.addGithubUser (UserName uname) ghId email
                makeTok
              e -> throwError e
    return tok

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

-- | This is a sub-api complete with handlers and frontend callbacks for github single-sign-on.
--
-- There are two rest end-points, one for requesting sso, and one for sso confirmation.  The
-- frontend needs two js callbacks provided from here: one that is registered as an on-click handler
-- for the "sign on with github" button and hits the "request" end-point; and one that handles the
-- return uri (which points into the service, not thentos), hits the "confirm" end-point with the
-- credentials stored in the return uri, and registers the resulting session token with the
-- frontend.
--
-- The gist of the protocol looks something like this:
--
-- >>> step req# description
-- >>>
-- >>> 01.  1.   BRO -> THT:   "can i give you access to my sso data on github so you know it's me?"
-- >>> 02.  1.   THT -> BRO:   "ok, here is a request token and a re-direct uri."
-- >>> 03.  2.   BRO -> GIH:   "THT gave me this request token."
-- >>> 04.  2.   GIH -> BRO:   "do you want to let THT access your github-verified email address?"
-- >>> 05.  3.   BRO -> GIH:   "sure!"
-- >>> 06.  3.   GIH -> BRO:   "ok, please pass this access token on to THT."
-- >>> 07.  4.   BRO -> THT:   "here you go: take this to github and check out my email address"
-- >>> 08.  5.     THT -> GIH: "here is an access token."
-- >>> 09.  5.     GIH -> THT: "here is some accessed information: email address."
-- >>> 10.  4.   THT  -> BRO:  "here is your 'ThentosSessionToken'."
--
-- Steps 1..2 are the request end-point, and result in aiming the browser at github.com.  Steps 3..6
-- happen between browser and github.com, and neither thentos nor the underlying service can see
-- these steps.
--
-- What happens in step 7 is actually a bit more complex than depicted here: the browser actually
-- hits the underlying service, not thentos (the return uri was originally provided by the service
-- as an argument to the request callback).  The confirmation callback in the browser will be
-- triggered because the application has made sure it is called at this point (e.g. by registering
-- it as a handler in the routing table in an angular single-page app).  It will hit the "confirm"
-- end-point, still in step 7; THT will talk to GIH for a bit, and return into the confirmation
-- callback in step 10.  The session token is registered in the service frontend (by passing it to
-- yet another callback function passed to the route handler as an argument).
--
-- SEE ALSO:
--
--   - https://developer.github.com/guides/basics-of-authentication/
--
--   - https://developer.github.com/v3/oauth/
--
--
-- TODO:
--
--   - move githubKey contents to config
--
--   - PR to hoauth2: split up OAuth2 type into two, one containing the callback (as a mandatory
--     value, not as Maybe), and one containing everything else.  change api so that there are fewer
--     partial functions.  also, base everything on uri-bytestring.
--
--   - 'Thentos.Config.exposeUrl' operates on 'ST', but should operate on 'URI'.  Go through entire
--     module there and find everything that needs to be made uri-bytestring-aware as well.
--
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
import Language.ECMAScript3.Syntax.QuasiQuote (js)
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.PrettyPrint
import Language.ECMAScript3.Parser
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding   as ST
import qualified Network.HTTP.Client  as Client
import qualified Network.HTTP.Conduit as Http

import System.Log.Missing
import Thentos.Action as A
import Thentos.Action.Core
import Thentos.Backend.Api.Proxy (ServiceProxy, serviceProxy)
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
    thentosHttp = Tagged . fromMaybe (error "thentosSso: requires backend config!") $
        cfg >>. (Proxy :: Proxy '["backend"])

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


-- * js

jsHandleRequest :: JavaScript SourcePos
jsHandleRequest = unsafePerformIO $ parseFromFile "./src/Thentos/Backend/Api/Sso.js"

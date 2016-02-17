{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

module Thentos.Adhocracy3.Action
    ( a3ServiceId
    , activate
    , addUser
    , externalUrlOfDefaultPersona
    , login
    , makeExternalUrl
    , resetPassword
    ) where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (SBS, ST, cs)
import System.Log (Priority(DEBUG))

import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Text as ST
import qualified URI.ByteString as URI

import Thentos.Adhocracy3.Action.Types
import Thentos.Action.TCB (loggerA)
import Thentos.Config
import Thentos.Types

import qualified Thentos.Action as A
import qualified Thentos.Action.TCB as A
import qualified Thentos.Action.Unsafe as U
import qualified Thentos.Adhocracy3.Action.Unsafe as U


-- | Add a user in Thentos, but not yet in A3. Only after the Thentos user has been activated
-- (confirmed), a persona is created in thentos together with a corresponding adhocracy user in A3
-- that corresponds to that persona.
addUser :: A3UserWithPass -> A3Action TypedPathWithCacheControl
addUser (A3UserWithPass user) = A.logIfError $ do
    loggerA DEBUG . ("route addUser: " <>) . cs .
        Aeson.encodePretty $ A3UserNoPass user
    A.addUnconfirmedUser user
    config <- A.getConfig
    let dummyPath = a3backendPath config ""
    return $ TypedPathWithCacheControl (TypedPath dummyPath CTUser) [] [] [] []

    -- FIXME Which path should we return here, considering that no A3 user exists yet?!

    -- FIXME We correctly return empty cache-control info since the A3 DB isn't (yet) changed,
    -- but normally the A3 backend would return the following info if a new user is created:
    -- changedDescendants: "", "principals/", "principals/users/", "principals/groups/"
    -- created: userPath
    -- modified: "principals/users/", "principals/groups/authenticated/"
    -- Find out if not returning this stuff leads to problems in the A3 frontend!
    --
    -- possible solution: deliver thentos registration widget; disable all adhocracy frontend-code
    -- that touches this end-point; provide user resources from outside of widgets only.

-- | Activate a new user. This also creates a persona and a corresponding adhocracy user in the A3 backend,
-- so that the user is able to log into A3. The user's actual password and email address are
-- only stored in Thentos and NOT exposed to A3.
activate :: ActivationRequest -> A3Action RequestResult
activate ar@(ActivationRequest confToken) = A.logIfError $ do
    loggerA DEBUG . ("route activate:" <>) . cs $ Aeson.encodePretty ar
    (uid, stok) <- A.confirmNewUser confToken
    -- Promote access rights so we can look up the user and create a persona
    U.extendClearanceOnAgent (UserA uid)
    user <- snd <$> A.lookupConfirmedUser uid
    let persName = PersonaName . fromUserName $ user ^. userName
    externalUrl <- makeExternalUrl persName
    persona     <- A.addPersona persName uid $ Just externalUrl
    sid         <- a3ServiceId
    -- Register persona for the default ("") context of the default service (A3)
    A.registerPersonaWithContext persona sid ""
    pure $ RequestSuccess (Path . cs . renderUri $ externalUrl) stok

-- | Make user path relative to our exposed URL instead of the proxied A3 backend URL.  Only works
-- for @/principals/users/...@.  (Returns exposed url.)
makeExternalUrl :: PersonaName -> A3Action Uri
makeExternalUrl pn = U.createUserInA3 pn >>= f
  where
    f :: Path -> A3Action Uri
    f (Path path@(ST.breakOn "/principals/users/" -> (_, localPath)))
        | ST.null localPath = do
            throwError . OtherError . A3UriParseError . URI.OtherError $ "bad A3 user uri: " <> cs path
        | otherwise = do
            config <- A.getConfig
            let (Path fullPath) = a3backendPath config localPath
            case parseUri $ cs fullPath  of
                Left err  -> throwError . OtherError $ A3UriParseError err
                Right uri -> pure uri

-- | Log a user in.
login :: LoginRequest -> A3Action RequestResult
login r = A.logIfError $ do
    loggerA DEBUG "/login/"
    (uid, stok) <- case r of
        LoginByName  uname pass -> A.startThentosSessionByUserName uname pass
        LoginByEmail email pass -> A.startThentosSessionByUserEmail email pass
    userUrl <- externalUrlOfDefaultPersona uid
    return $ RequestSuccess (Path $ cs userUrl) stok

-- | Finish password reset with email confirmation and open a new ThentosSession for the user.
resetPassword :: PasswordResetRequest -> A3Action RequestResult
resetPassword (PasswordResetRequest resetTok pass) = A.logIfError $ do
    loggerA DEBUG $ "route password_reset for token: " <> show resetTok
    uid <- A.resetPassword resetTok pass
    sessTok <- A.startThentosSessionByUserId uid pass
    userUrl <- externalUrlOfDefaultPersona uid
    return $ RequestSuccess (Path $ cs userUrl) sessTok

-- | Convert a local file name into a absolute path relative to the A3 backend endpoint.  (Returns
-- exposed url.)
a3backendPath :: ThentosConfig -> ST -> Path
a3backendPath config localPath = Path $ cs (exposeUrl beHttp) <//> localPath
  where
    beHttp     = case config >>. (Proxy :: Proxy '["backend"]) of
                     Nothing -> error "a3backendPath: backend not configured!"
                     Just v -> Tagged v

-- * helper actions

-- | Find the ServiceId of the A3 backend, which should be registered as default proxied app.
a3ServiceId :: A3Action ServiceId
a3ServiceId = do
    config  <- A.getConfig
    maybe (error "a3ServiceId: A3 proxy not configured") return $
        ServiceId <$> config >>. (Proxy :: Proxy '["proxy", "service_id"])

-- | Return the external URL of a user's default ("") persona, in rendered form.
externalUrlOfDefaultPersona :: UserId -> A3Action SBS
externalUrlOfDefaultPersona uid = do
    sid     <- a3ServiceId
    persona <- A.findPersona uid sid "" >>=
               maybe (throwError . OtherError $ A3NoDefaultPersona uid sid) pure
    userUrl <- maybe (throwError $ OtherError A3PersonaLacksExternalUrl) pure $
                     persona ^. personaExternalUrl
    pure $ renderUri userUrl

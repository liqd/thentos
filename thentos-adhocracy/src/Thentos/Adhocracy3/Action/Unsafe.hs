{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}

module Thentos.Adhocracy3.Action.Unsafe
    ( createUserInA3
    ) where

import Control.Monad.Except (throwError)
import Control.Monad (when)
import Data.Aeson (ToJSON)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (LBS, cs)
import LIO.Core (liftLIO)

import LIO.TCB (ioTCB)
import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types.Status as Status

import Thentos.Action.TCB
import Thentos.Adhocracy3.Action.Types
import Thentos.Config
import Thentos.Types
import Thentos.Util


-- | Create a user in A3 from a persona name and return the user path.
createUserInA3 :: PersonaName -> A3Action Path
createUserInA3 persName = do
    config <- getConfig
    let a3req = fromMaybe
                (error "createUserInA3: mkUserCreationRequestForA3 failed, check config!") $
                mkUserCreationRequestForA3 config persName
    a3resp <- liftLIO . ioTCB . sendRequest $ a3req
    when (responseCode a3resp >= 400) $ do
        throwError . OtherError . A3BackendErrorResponse (responseCode a3resp) $
            Client.responseBody a3resp
    extractUserPath a3resp
  where
    responseCode = Status.statusCode . Client.responseStatus

-- | Convert a persona name into a user creation request to be sent to the A3 backend.
-- The persona name is used as user name. The email address is set to a unique dummy value
-- and the password is set to a dummy value.
mkUserCreationRequestForA3 :: ThentosConfig -> PersonaName -> Maybe Client.Request
mkUserCreationRequestForA3 config persName = do
    let user  = UserFormData { udName     = UserName $ fromPersonaName persName,
                               udEmail    = email,
                               udPassword = "dummypass" }
    mkRequestForA3 config "/principals/users" $ A3UserWithPass user
  where
    rawEmail = cs (mailEncode $ fromPersonaName persName) <> "@example.org"
    email    = fromMaybe (error $ "mkUserCreationRequestForA3: couldn't create dummy email") $
                         parseUserEmail rawEmail

-- | Make a POST request to be sent to the A3 backend. Returns 'Nothing' if the 'ThentosConfig'
-- lacks a correctly configured proxy.
--
-- Note that the request is configured to NOT thrown an exception even if the response status code
-- indicates an error (400 or higher). Properly dealing with error replies is left to the caller.
--
-- Since the A3 frontend doesn't know about different services (i.e. never sends a
-- @X-Thentos-Service@ header), we send the request to the default proxy which should be the A3
-- backend.
mkRequestForA3 :: ToJSON a => ThentosConfig -> String -> a -> Maybe Client.Request
mkRequestForA3 config route dat = do
    defaultProxy <- Tagged <$> config >>. (Proxy :: Proxy '["proxy"])
    let target = extractTargetUrl defaultProxy
    initReq <- Client.parseUrl $ cs $ show target <//> route
    return initReq { Client.method = "POST"
        , Client.requestHeaders = [("Content-Type", "application/json")]
        , Client.requestBody = Client.RequestBodyLBS . Aeson.encode $ dat
        , Client.checkStatus = \_ _ _ -> Nothing
        }

-- | Extract the user path from an A3 response received for a user creation request.
-- FIXME: make use of servant-client for all rest communication with A3 backend!
extractUserPath :: MonadThentosError ThentosA3Error m => Client.Response LBS -> m Path
extractUserPath resp = do
    resource <- either (throwError . OtherError . A3BackendInvalidJson) return $
        (Aeson.eitherDecode . Client.responseBody $ resp :: Either String TypedPath)
    pure $ tpPath resource

sendRequest :: Client.Request -> IO (Client.Response LBS)
sendRequest req = Client.newManager Client.defaultManagerSettings >>= Client.httpLbs req

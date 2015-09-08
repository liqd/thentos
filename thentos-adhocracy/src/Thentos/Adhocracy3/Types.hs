{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE UndecidableInstances        #-}


module Thentos.Adhocracy3.Types
    ( module Thentos.Types
    , DB(..)
    , dbCoreDB
    , dbSsoTokens
    , SsoToken(..)
    , ThentosError(..)
    , A3Error(..)
    , A3ErrorMessage(..)
    , mkSimpleA3Error
    )
    where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (Exception)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (parseJSON), ToJSON(toJSON), Value(String), (.=), (.:), encode,
                   object, withObject)
import Data.Data (Typeable)
import Data.Maybe (fromMaybe)
import Data.SafeCopy (SafeCopy, deriveSafeCopy, base, putCopy, getCopy)
import Data.Set (Set)
import Data.String.Conversions (LBS, ST)
import Data.Thyme.Time () -- required for NominalDiffTime's num instance
import GHC.Generics (Generic)
import Servant.Server (ServantErr)
import Servant.Server.Internal.ServantErr (err400, err500, errBody, errHeaders)
import System.Log.Logger (Priority(ERROR))

import Thentos.Backend.Core
import Thentos.Types hiding (DB)

import qualified Data.Set as Set
import qualified Thentos.Action.Core as AC
import qualified Thentos.Types

data DB = DB
    { _dbCoreDB    :: !Thentos.Types.DB
    , _dbSsoTokens :: !(Set SsoToken)
    } deriving (Eq, Show, Typeable, Generic)

instance EmptyDB DB where
    emptyDB = DB emptyDB Set.empty

instance DB `Extends` Thentos.Types.DB where
    focus f (DB db ssoTokens) = flip DB ssoTokens <$> f db
    thentosErrorFromParent = ThentosA3ErrorCore
    thentosErrorToParent (ThentosA3ErrorCore e) = Just e
    thentosErrorToParent _ = Nothing

instance DB `Extends` DB where
    focus = id
    thentosErrorFromParent = id
    thentosErrorToParent = Just

data instance ThentosError DB =
      ThentosA3ErrorCore (ThentosError Thentos.Types.DB)
    | GenericA3Error A3ErrorMessage
    | A3BackendErrorResponse Int LBS
    | A3BackendInvalidJson String
    | SsoErrorUnknownCsrfToken
    | SsoErrorCouldNotAccessUserInfo LBS
    | SsoErrorCouldNotGetAccessToken LBS

deriving instance Eq (ThentosError DB)
deriving instance Show (ThentosError DB)
deriving instance Read (ThentosError DB)

instance Exception (ThentosError DB)

deriving instance Show (AC.ActionError DB)

instance SafeCopy (ThentosError DB)
  where
    putCopy = putCopyViaShowRead
    getCopy = getCopyViaShowRead

-- | Wraps the error details reported by A3.
data A3Error = A3Error
    { aeName        :: ST
    , aeLocation    :: ST
    , aeDescription :: ST
    } deriving (Eq, Read, Show)

instance ToJSON A3Error where
    toJSON err = object
            [ "name"        .= aeName err
            , "location"    .= aeLocation err
            , "description" .= aeDescription err
            ]

instance FromJSON A3Error where
    parseJSON = withObject "A3-style error description" $ \v -> A3Error
        <$> (v .: "name")
        <*> (v .: "location")
        <*> (v .: "description")

-- | An A3-style error message contains a list of errors.
newtype A3ErrorMessage = A3ErrorMessage { a3errors :: [A3Error] }
    deriving (Eq, Read, Show)

instance ToJSON A3ErrorMessage where
    toJSON (A3ErrorMessage es) = object
        [ "status" .= String "error"
        , "errors" .= es
        ]

instance FromJSON A3ErrorMessage where
    parseJSON = withObject "A3-style error message" $ \v -> A3ErrorMessage <$> (v .: "errors")

-- Construct a simple A3-style error wrapping a single error. 'aeName' is set to "thentos" and
-- 'aeLocation' to "body". Useful for cases where all we really have is a description.
mkSimpleA3Error :: ST -> A3Error
mkSimpleA3Error desc = A3Error {aeName = "thentos", aeLocation = "body", aeDescription = desc}

-- | Construct a ServantErr that looks like those reported by the A3 backend.
-- The backend returns a list of errors but we always use a single-element list, as Thentos
-- aborts at the first detected error.
mkA3StyleServantErr :: ServantErr -> A3Error -> ServantErr
mkA3StyleServantErr baseErr err = baseErr
    {errBody = encode $ A3ErrorMessage [err], errHeaders = [contentTypeJsonHeader]}

-- | Construct a simple A3-style 'ServantErr' from only a base error and a message, setting the
-- other A3-specific fields to default values.
mkSimpleA3StyleServantErr :: MkServantErrFun
mkSimpleA3StyleServantErr baseErr msg = mkA3StyleServantErr baseErr $ mkSimpleA3Error msg

instance ThentosErrorToServantErr DB where
    thentosErrorToServantErr mMkErr = f
      where
        mkErr = fromMaybe mkSimpleA3StyleServantErr mMkErr

        -- For errors specifically relevant to the A3 frontend we mirror the A3 backend errors
        -- exactly so that the frontend recognizes them
        f (ThentosA3ErrorCore e) = case e of
            BadCredentials -> (Nothing, mkA3StyleServantErr err400 $ A3Error
                "password"
                "body"
                "User doesn't exist or password is wrong")
            UserEmailAlreadyExists -> (Nothing, mkA3StyleServantErr err400 $ A3Error
                "data.adhocracy_core.sheets.principal.IUserExtended.email"
                "body"
                "The user login email is not unique")
            UserNameAlreadyExists -> (Nothing, mkA3StyleServantErr err400 $ A3Error
                "data.adhocracy_core.sheets.principal.IUserBasic.name"
                "body"
                "The user login name is not unique")
            NoSuchPendingUserConfirmation -> (Nothing, mkA3StyleServantErr err400 $ A3Error
                "path"
                "body"
                "Unknown or expired activation path")
            NoSuchThentosSession -> (Nothing, mkA3StyleServantErr err400 $ A3Error
                "X-User-Token"
                "header"
                "Invalid user token")
            _ -> thentosErrorToServantErr (Just mkErr) e

        f (GenericA3Error errMsg) =
            (Nothing, err400 {errBody = encode errMsg, errHeaders = [contentTypeJsonHeader]} )
        f e@(A3BackendErrorResponse _ _) =
            (Just (ERROR, show e), mkErr err500 "exception in a3 backend")
        f e@(A3BackendInvalidJson _) =
            (Just (ERROR, show e), mkErr err500 "exception in a3 backend: received bad json")
        -- the following shouldn't actually reach servant:
        f e@SsoErrorUnknownCsrfToken =
            (Just (ERROR, show e), mkErr err500 "invalid token returned during sso process")
        f e@(SsoErrorCouldNotAccessUserInfo _) =
            (Just (ERROR, show e), mkErr err500 "error accessing user info")
        f e@(SsoErrorCouldNotGetAccessToken _) =
            (Just (ERROR, show e), mkErr err500 "error retrieving access token")

newtype SsoToken = SsoToken { fromSsoToken :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''DB

$(deriveSafeCopy 0 'base ''SsoToken)
$(deriveSafeCopy 0 'base ''DB)

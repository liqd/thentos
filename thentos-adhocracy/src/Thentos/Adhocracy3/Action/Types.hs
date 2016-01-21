{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}


module Thentos.Adhocracy3.Action.Types where

import Control.Exception (Exception)
import Control.Monad (when, mzero)
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), Value(Object), (.:), (.:?), (.=), object, withObject)
import Data.Data (Typeable)
import Data.Functor.Infix ((<$$>))
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.String.Conversions (LBS, ST, cs)
import Data.Thyme.Time () -- required for NominalDiffTime's num instance
import GHC.Generics (Generic)
import Safe (readMay)
import URI.ByteString (URIParseError)

import Thentos.Backend.Api.Docs.Proxy ()
import Thentos.Types
import Thentos.Util

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as ST
import qualified Thentos.Action.Types (Action)
import qualified Thentos.Backend.Api.Simple ()


data ThentosA3Error =
      GenericA3Error A3ErrorMessage
    | A3BackendErrorResponse Int LBS
    | A3BackendInvalidJson String
    | A3UriParseError URIParseError
    | A3NoDefaultPersona UserId ServiceId
    | A3PersonaLacksExternalUrl
  deriving (Eq, Show, Read, Typeable)

instance Exception ThentosA3Error

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
        [ "status" .= Aeson.String "error"
        , "errors" .= es
        ]

instance FromJSON A3ErrorMessage where
    parseJSON = withObject "A3-style error message" $ \v -> A3ErrorMessage <$> (v .: "errors")

type A3Action = Thentos.Action.Types.Action ThentosA3Error ()


-- * data types

-- ** basics

-- | A "Path" here identifies an a3 Ressource, not a file system path.
newtype Path = Path { fromPath :: ST }
  deriving (Eq, Ord, Show, Read, Typeable, Generic, FromJSON, ToJSON)

data ContentType = CTUser
  deriving (Eq, Ord, Enum, Bounded, Typeable, Generic)

instance Show ContentType where
    show CTUser = "adhocracy_core.resources.principal.IUser"

instance Read ContentType where
    readsPrec = readsPrecEnumBoundedShow

instance ToJSON ContentType where
    toJSON = Aeson.String . cs . show

instance FromJSON ContentType where
    parseJSON = Aeson.withText "content type string" $ maybe (fail "invalid content type") return . readMay . cs

data PropertySheet =
      PSUserBasic
    | PSUserExtended
    | PSPasswordAuthentication
  deriving (Eq, Enum, Bounded, Typeable)

instance Show PropertySheet where
    show PSUserBasic              = "adhocracy_core.sheets.principal.IUserBasic"
    show PSUserExtended           = "adhocracy_core.sheets.principal.IUserExtended"
    show PSPasswordAuthentication = "adhocracy_core.sheets.principal.IPasswordAuthentication"

instance Read PropertySheet where
    readsPrec = readsPrecEnumBoundedShow


-- ** resource

data A3Resource a = A3Resource
  { mPath :: Maybe Path
  , mContentType :: Maybe ContentType
  , mData :: Maybe a
  } deriving (Eq, Show, Typeable, Generic)

instance ToJSON a => ToJSON (A3Resource a) where
    toJSON (A3Resource p ct r) =
        object $ "path" .= p : "content_type" .= ct : case Aeson.toJSON <$> r of
            Just (Object v) -> HashMap.toList v
            Nothing -> []
            Just _ -> []

instance FromJSON a => FromJSON (A3Resource a) where
    parseJSON = withObject "resource object" $ \v -> A3Resource
        <$> (v .:? "path")
        <*> (v .:? "content_type")
        <*> (v .:? "data")

-- | Similar to A3Resource, but tailored for cases where @path@ and @content_type@ are present and
-- @data@ is absent (or irrelevant).
data TypedPath = TypedPath
  { tpPath :: Path
  , tpContentType :: ContentType
  } deriving (Eq, Show, Generic)

instance ToJSON TypedPath where
    toJSON (TypedPath p ct) = object ["path" .= p, "content_type" .= ct]

instance FromJSON TypedPath where
    parseJSON = withObject "resource object" $ \v -> TypedPath
        <$> (v .: "path") <*> (v .: "content_type")

-- | The A3 backend replies to POST request not just with a typed path, but also sends a listing
-- of created/modified etc. resources along to allow the A3 frontend to update its cache.
data TypedPathWithCacheControl = TypedPathWithCacheControl
    { tpccTypedPath :: TypedPath
    , tpccChangedDescendants :: [Path]
    , tpccCreated :: [Path]
    , tpccModified :: [Path]
    , tpccRemoved :: [Path]
    } deriving (Eq, Show, Generic)

instance ToJSON TypedPathWithCacheControl where
    toJSON t = object
        [ "path"              .= tpPath (tpccTypedPath t)
        , "content_type"      .= tpContentType (tpccTypedPath t)
        , "updated_resources" .= object
            [ "changed_descendants" .= tpccChangedDescendants t
            , "created"             .= tpccCreated t
            , "modified"            .= tpccModified t
            , "removed"             .= tpccRemoved t
            ]
        ]


-- ** individual resources

newtype A3UserNoPass = A3UserNoPass { fromA3UserNoPass :: UserFormData }
  deriving (Eq, Typeable, Generic)

newtype A3UserWithPass = A3UserWithPass { fromA3UserWithPass :: UserFormData }
  deriving (Eq, Typeable, Generic)

instance ToJSON A3UserNoPass where
    toJSON (A3UserNoPass user) = a3UserToJSON False user

instance ToJSON A3UserWithPass where
    toJSON (A3UserWithPass user) = a3UserToJSON True user

instance FromJSON A3UserNoPass where
    parseJSON value = A3UserNoPass <$> a3UserFromJSON False value

instance FromJSON A3UserWithPass where
    parseJSON value = A3UserWithPass <$> a3UserFromJSON True value

a3UserToJSON :: Bool -> UserFormData -> Aeson.Value
a3UserToJSON withPass (UserFormData name password email) = object
    [ "content_type" .= CTUser
    , "data" .= object (catMaybes
        [ Just $ cshow PSUserBasic .= object
            [ "name" .= name
            ]
        , Just $ cshow PSUserExtended .= object
            [ "email" .= email
            ]
        , if withPass
            then Just $ cshow PSPasswordAuthentication .= object ["password" .= password]
            else Nothing
        ])
    ]

a3UserFromJSON :: Bool -> Aeson.Value -> Aeson.Parser UserFormData
a3UserFromJSON withPass = withObject "resource object" $ \v -> do
    content_type :: ContentType <- v .: "content_type"
    when (content_type /= CTUser) $
        fail $ "wrong content type: " ++ show content_type
    name     <- v .: "data" >>= (.: cshow PSUserBasic) >>= (.: "name")
    email    <- v .: "data" >>= (.: cshow PSUserExtended) >>= (.: "email")
    password <- if withPass
        then v .: "data" >>= (.: cshow PSPasswordAuthentication) >>= (.: "password")
        else pure ""
    failOnError $ userNameValid name
    when withPass . failOnError $ passwordAcceptable password
    return $ UserFormData (UserName name) (UserPass password) email

-- | Fail if the argument is 'Just' an error. Do nothing otherwise.
failOnError :: Monad m => Maybe String -> m ()
failOnError = maybe (return ()) fail

-- | Check constraints on user name: The "name" field in the "IUserBasic"
-- schema is a non-empty string that can contain any characters except
-- '@' (to make user names distinguishable from email addresses). The
-- username must not contain any whitespace except single spaces,
-- preceded and followed by non-whitespace (no whitespace at begin or
-- end, multiple subsequent spaces are forbidden, tabs and newlines
-- are forbidden).
-- Returns 'Nothing' on success, otherwise 'Just' an error message.
userNameValid :: ST -> Maybe String
userNameValid name
  | ST.null name           = Just "user name is empty"
  | ST.any (== '@') name   = Just $ "'@' in user name is not allowed: "  ++ show name
  | normalizedName /= name = Just $ "Illegal whitespace sequence in user name: "  ++ show name
  | otherwise              = Nothing
  where normalizedName = ST.unwords . ST.words $ name

-- | Check constraints on password: It must have between 6 and 100 chars.
-- Returns 'Nothing' on success, otherwise 'Just' an error message.
passwordAcceptable :: ST -> Maybe String
passwordAcceptable pass
  | len < 6   = Just "password too short (less than 6 characters)"
  | len > 100 = Just "password too long (more than 100 characters)"
  | otherwise = Nothing
  where len = ST.length pass


-- ** other types

data ActivationRequest =
    ActivationRequest ConfirmationToken
  deriving (Eq, Show, Typeable, Generic)

data LoginRequest =
    LoginByName UserName UserPass
  | LoginByEmail UserEmail UserPass
  deriving (Eq, Typeable, Generic)

-- | Reponse to a login or similar request.
--
-- Note: this generic form is useful for parsing a result from A3, but you should never *return* a
-- 'RequestError' because then the status code won't be set correctly. Instead, throw a
-- 'GenericA3Error' or, even better, define and throw a custom error.
data RequestResult =
    RequestSuccess Path ThentosSessionToken
  | RequestError A3ErrorMessage
  deriving (Eq, Show, Typeable, Generic)

instance ToJSON ActivationRequest where
    toJSON (ActivationRequest (ConfirmationToken confToken)) =
        object ["path" .= ("/activate/" <> confToken)]

instance FromJSON ActivationRequest where
    parseJSON = withObject "activation request" $ \v -> do
        p :: ST <- v .: "path"
        case ST.stripPrefix "/activate/" p of
            Just confToken -> return . ActivationRequest . ConfirmationToken $ confToken
            Nothing        -> fail $ "ActivationRequest with malformed path: " ++ show p

instance ToJSON LoginRequest where
    toJSON (LoginByName  n p) = object ["name"  .= n, "password" .= p]
    toJSON (LoginByEmail e p) = object ["email" .= e, "password" .= p]

instance FromJSON LoginRequest where
    parseJSON = withObject "login request" $ \v -> do
        name <- UserName  <$$> v .:? "name"
        email <- v .:? "email"
        pass <- UserPass <$>  v .: "password"
        case (name, email) of
          (Just x,  Nothing) -> return $ LoginByName x pass
          (Nothing, Just x)  -> return $ LoginByEmail x pass
          (_,       _)       -> fail $ "malformed login request body: " ++ show v

instance ToJSON RequestResult where
    toJSON (RequestSuccess p t) = object $
        "status" .= ("success" :: ST) :
        "user_path" .= p :
        "user_token" .= t :
        []
    toJSON (RequestError errMsg) = toJSON errMsg

instance FromJSON RequestResult where
    parseJSON = withObject "request result" $ \v -> do
        n :: ST <- v .: "status"
        case n of
            "success" -> RequestSuccess <$> v .: "user_path" <*> v .: "user_token"
            "error" -> RequestError <$> parseJSON (Object v)
            _ -> mzero

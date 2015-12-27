{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE UndecidableInstances        #-}


module Thentos.Adhocracy3.Types
    ( module Thentos.Types
    , ThentosA3Error(..)
    , A3ErrorMessage(..)
    , A3Error(..)
    , A3Action
    )
    where

import Control.Exception (Exception)
import Data.Aeson (FromJSON (parseJSON), ToJSON(toJSON), Value(String), (.=), (.:),
                   object, withObject)
import Data.Data (Typeable)
import Data.String.Conversions (LBS, ST)
import Data.Thyme.Time () -- required for NominalDiffTime's num instance
import URI.ByteString (URIParseError)

import Thentos.Types

import qualified Thentos.Action.Types (Action)


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
        [ "status" .= String "error"
        , "errors" .= es
        ]

instance FromJSON A3ErrorMessage where
    parseJSON = withObject "A3-style error message" $ \v -> A3ErrorMessage <$> (v .: "errors")

type A3Action = Thentos.Action.Types.Action ThentosA3Error ()

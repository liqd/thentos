{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}

module Thentos.Backend.Api.Auth.Types
where

import GHC.Generics (Generic)
import LIO.DCLabel (ToCNF, toCNF)
import Network.Socket (SockAddr)

import Thentos.Types (ThentosSessionToken)


data ThentosAuth

data ThentosAuthCredentials =
    ThentosAuthCredentials
        { thentosAuthSessionToken :: Maybe ThentosSessionToken
        , thentosAuthOrigin       :: SockAddr
        }

-- | Principal for lio 'CNF' expressions that is present iff request's IP address is privileged.
data PrivilegedIP = PrivilegedIP
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance ToCNF PrivilegedIP where toCNF = toCNF . show

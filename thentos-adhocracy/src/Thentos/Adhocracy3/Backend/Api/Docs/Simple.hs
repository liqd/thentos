{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE OverloadedStrings                        #-}

{-# OPTIONS -fno-warn-orphans #-}

module Thentos.Adhocracy3.Backend.Api.Docs.Simple where

import Data.Proxy (Proxy(Proxy))
import Servant.Docs (ToSample(toSamples))

import qualified Servant.Docs as Docs

import Thentos.Backend.Api.Docs.Common ()
import Thentos.Backend.Api.Docs.Proxy ()

import qualified Thentos.Adhocracy3.Backend.Api.Simple as A3


instance ToSample A3.A3UserNoPass

instance ToSample A3.A3UserWithPass

instance ToSample a => ToSample (A3.A3Resource a)

instance ToSample A3.TypedPath

instance ToSample A3.Path where
    toSamples _ = Docs.singleSample $ A3.Path "/proposals/environment"

instance ToSample A3.TypedPathWithCacheControl

instance ToSample A3.ActivationRequest

-- FIXME: split up LoginRequest into two separate types for login by email
-- and login by user name, in order to provide a correct example for
-- login_email request body
instance ToSample A3.LoginRequest

instance ToSample A3.RequestResult where
    toSamples _ = [ ("Success", A3.RequestSuccess (A3.Path "somepath") "sometoken")]

instance ToSample A3.PasswordResetRequest

instance ToSample A3.ContentType where
    toSamples _ = Docs.singleSample A3.CTUser

docs :: Docs.API
docs = Docs.docs (Proxy :: Proxy A3.Api)

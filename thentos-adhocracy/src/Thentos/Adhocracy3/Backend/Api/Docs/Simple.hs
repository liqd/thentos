{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE OverloadedStrings                        #-}

{-# OPTIONS -fno-warn-orphans #-}

module Thentos.Adhocracy3.Backend.Api.Docs.Simple where

import Control.Applicative (pure, (<$>), (<*>))
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy(Proxy))
import Servant.Docs (ToSample(toSample))

import qualified Servant.Docs as Docs

import Thentos.Backend.Api.Docs.Common ()
import Thentos.Backend.Api.Docs.Proxy ()
import Thentos.Types

import qualified Thentos.Adhocracy3.Backend.Api.Simple as A3


instance ToSample A3.A3UserNoPass A3.A3UserNoPass where
    toSample _ = A3.A3UserNoPass <$> toSample (Proxy :: Proxy UserFormData)

instance ToSample A3.A3UserWithPass A3.A3UserWithPass where
    toSample _ = A3.A3UserWithPass <$> toSample (Proxy :: Proxy UserFormData)

instance ToSample a a => ToSample (A3.A3Resource a) (A3.A3Resource a) where
    toSample _ = A3.A3Resource
                    <$> (Just <$> toSample (Proxy :: Proxy A3.Path))
                    <*> (Just <$> toSample (Proxy :: Proxy A3.ContentType))
                    <*> (Just <$> toSample (Proxy :: Proxy a))

instance ToSample A3.TypedPath A3.TypedPath where
    toSample _ = A3.TypedPath
                    <$> toSample (Proxy :: Proxy A3.Path)
                    <*> toSample (Proxy :: Proxy A3.ContentType)

instance ToSample A3.Path A3.Path where
    toSample _ = pure $ A3.Path "/proposals/environment"

instance ToSample [A3.Path] [A3.Path] where
    toSample _ = pure . maybeToList $ toSample (Proxy :: Proxy A3.Path)

instance ToSample A3.TypedPathWithCacheControl A3.TypedPathWithCacheControl where
    toSample _ = A3.TypedPathWithCacheControl
                     <$> toSample (Proxy :: Proxy A3.TypedPath)
                     <*> toSample (Proxy :: Proxy [A3.Path])
                     <*> toSample (Proxy :: Proxy [A3.Path])
                     <*> toSample (Proxy :: Proxy [A3.Path])
                     <*> toSample (Proxy :: Proxy [A3.Path])

instance ToSample A3.ActivationRequest A3.ActivationRequest where
    toSample _ = A3.ActivationRequest <$> toSample (Proxy :: Proxy A3.Path)

-- FIXME: split up LoginRequest into two separate types for login by email
-- and login by user name, in order to provide a correct example for
-- login_email request body
instance ToSample A3.LoginRequest A3.LoginRequest where
    toSample _ = A3.LoginByName <$> toSample (Proxy :: Proxy UserName)
                                <*> toSample (Proxy :: Proxy UserPass)

instance ToSample A3.PasswordResetRequest A3.PasswordResetRequest where
    toSample _ = A3.PasswordResetRequest
                    <$> toSample (Proxy :: Proxy A3.Path)
                    <*> toSample (Proxy :: Proxy UserPass)

instance ToSample A3.RequestResult A3.RequestResult where
    toSample _ = A3.RequestSuccess
                    <$> toSample (Proxy :: Proxy A3.Path)
                    <*> toSample (Proxy :: Proxy ThentosSessionToken)

instance ToSample A3.ContentType A3.ContentType where
    toSample _ = pure A3.CTUser

docs :: Docs.API
docs = Docs.docs (Proxy :: Proxy A3.Api)

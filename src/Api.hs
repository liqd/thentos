{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving               #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE NoImplicitPrelude                        #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS -fno-warn-unused-imports -fwarn-incomplete-patterns -fdefer-type-errors #-}

module Api
where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Either
import Data.Acid (AcidState)
import Data.Acid.Advanced (query', update')
import Data.Data
import Data.Function
import Data.Map (Map)
import Data.Maybe
import Data.SafeCopy
import Data.String.Conversions
import GHC.Generics
import Prelude
import Safe
import Servant.API
import Servant.Server
import System.Environment

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Map as Map

import DB
import Types


type App = ThentosBasic

type ThentosBasic =
       "user" :> ThentosUser
--  :<|> "service" :> ThentosService

app :: AcidState DB -> Server App
app st = getUserIds st :<|> getUser st :<|> postNewUser st -- :<|> postNamedUser


-- * user

type ThentosUser =
       Get [UserID]
  :<|> Capture "userid" Int :> Get User
  :<|> ReqBody User :> Post User
--  :<|> Capture "name" ST :> ReqBody User :> Post User

getUserIds :: AcidState DB -> EitherT (Int, String) IO [UserID]
getUserIds st = liftIO $ query' st AllUserIDs

getUser :: AcidState DB -> Int -> EitherT (Int, String) IO User
getUser st pname = liftIO (query' st (LookupUser pname)) >>= maybe noSuchUser right

postNewUser :: AcidState DB -> User -> EitherT (Int, String) IO User
postNewUser st proposal = liftIO $ do
  pname <- update' st (AddUser proposal)
  Just proposal' <- query' st (LookupUser pname)
  return proposal'

--    postNamedUser :: ST -> User -> EitherT (Int, String) IO User
--    postNamedUser pname proposal = _


-- * service

type ThentosService =
       "service" :> Get [ServiceID]
  :<|> "service" :> Capture "name" ST :> Get Service
  :<|> "service" :> ReqBody Service :> Post Service
  :<|> "service" :> Capture "name" ST :> ReqBody Service :> Post Service



-- * helpers

noSuchUser :: EitherT (Int, String) IO a
noSuchUser = left (404, "no such user")  -- FIXME: correct status code?

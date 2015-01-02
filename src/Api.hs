{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving               #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS  #-}

module Api
where

import Control.Monad.State (liftIO)
import Control.Monad.Trans.Either (EitherT, right, left)
import Data.Acid (AcidState)
import Data.Acid.Advanced (query', update')
import Data.Map (Map)
import Data.String.Conversions (ST)
import Servant.API ((:<|>)((:<|>)), (:>), Get, Post, Capture, ReqBody)
import Servant.Server (Server)

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

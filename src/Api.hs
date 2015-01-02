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
import Servant.API ((:<|>)((:<|>)), (:>), Get, Post, Put, Delete, Capture, ReqBody)
import Servant.Server (Server)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Map as Map

import DB (AllUserIDs(..), LookupUser(..), AddUser(..), UpdateUser(..), DeleteUser(..))
import Types


type App = ThentosBasic

type ThentosBasic =
       "user" :> ThentosUser
--  :<|> "service" :> ThentosService

app :: AcidState DB -> Server App
app st = thentosUser st


-- * user

type ThentosUser =
       Get [UserID]
  :<|> Capture "userid" UserID :> Get User
  :<|> ReqBody User :> Post UserID
  :<|> Capture "userid" UserID :> ReqBody User :> Put ()
  :<|> Capture "userid" UserID :> Delete

thentosUser st =
       getUserIds st
  :<|> getUser st
  :<|> postNewUser st
  :<|> postNamedUser st
  :<|> deleteUser st

getUserIds :: AcidState DB -> EitherT (Int, String) IO [UserID]
getUserIds st = liftIO $ query' st AllUserIDs

getUser :: AcidState DB -> UserID -> EitherT (Int, String) IO User
getUser st uid = liftIO (query' st (LookupUser uid)) >>= maybe noSuchUser right

postNewUser :: AcidState DB -> User -> EitherT (Int, String) IO UserID
postNewUser st user = liftIO $ update' st (AddUser user)

postNamedUser :: AcidState DB -> UserID -> User -> EitherT (Int, String) IO ()
postNamedUser st uid user = liftIO $ update' st (UpdateUser uid user)

deleteUser :: AcidState DB -> UserID -> EitherT (Int, String) IO ()
deleteUser st uid = liftIO $ update' st (DeleteUser uid)


-- * service

type ThentosService =
       "service" :> Get [ServiceID]
  :<|> "service" :> Capture "name" ST :> Get Service
  :<|> "service" :> ReqBody Service :> Post Service
  :<|> "service" :> Capture "name" ST :> ReqBody Service :> Post Service



-- * helpers

noSuchUser :: EitherT (Int, String) IO a
noSuchUser = left (404, "no such user")  -- FIXME: correct status code?

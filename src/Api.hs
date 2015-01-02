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

import DB
import Types


type App = ThentosBasic

type ThentosBasic =
       "user" :> ThentosUser
  :<|> "service" :> ThentosService

app :: AcidState DB -> Server App
app st =
       thentosUser st
  :<|> thentosService st


-- * user

type ThentosUser =
       Get [UserId]
  :<|> Capture "userid" UserId :> Get User
  :<|> Capture "userid" UserId :> ReqBody User :> Put ()
  :<|> ReqBody User :> Post UserId
  :<|> Capture "userid" UserId :> Delete

thentosUser st =
       getUserIds st
  :<|> getUser st
  :<|> postNamedUser st
  :<|> postNewUser st
  :<|> deleteUser st

getUserIds :: AcidState DB -> EitherT (Int, String) IO [UserId]
getUserIds st = liftIO $ query' st AllUserIDs

getUser :: AcidState DB -> UserId -> EitherT (Int, String) IO User
getUser st uid = liftIO (query' st (LookupUser uid)) >>= maybe noSuchUser right

postNewUser :: AcidState DB -> User -> EitherT (Int, String) IO UserId
postNewUser st = liftIO . update' st . AddUser

postNamedUser :: AcidState DB -> UserId -> User -> EitherT (Int, String) IO ()
postNamedUser st uid user = liftIO $ update' st (UpdateUser uid user)

deleteUser :: AcidState DB -> UserId -> EitherT (Int, String) IO ()
deleteUser st = liftIO . update' st . DeleteUser


-- * service

type ThentosService =
       Get [ServiceId]
  :<|> Capture "name" ST :> Get Service
  :<|> Post ServiceId

thentosService st =
         getServiceIds st
    :<|> getService st
    :<|> postNewService st

getServiceIds :: AcidState DB -> EitherT (Int, String) IO [ServiceId]
getServiceIds st = liftIO $ query' st AllServiceIDs

getService :: AcidState DB -> ST -> EitherT (Int, String) IO Service
getService st id =
    liftIO (query' st (LookupService id)) >>= maybe noSuchService right

postNewService :: AcidState DB -> EitherT (Int, String) IO ServiceId
postNewService st = liftIO $ update' st AddService


-- * helpers

noSuchUser :: EitherT (Int, String) IO a
noSuchUser = left (404, "no such user")  -- FIXME: correct status code?

noSuchService :: EitherT (Int, String) IO a
noSuchService = left (404, "no such service")

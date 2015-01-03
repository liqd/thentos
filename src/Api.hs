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
import Control.Monad.Trans.Either (right, left)
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

getUserIds :: AcidState DB -> RestAction [UserId]
getUserIds st = liftIO $ query' st AllUserIDs

getUser :: AcidState DB -> UserId -> RestAction User
getUser st uid = liftIO (query' st (LookupUser uid)) >>= maybe noSuchUser right

postNewUser :: AcidState DB -> User -> RestAction UserId
postNewUser st = liftIO . update' st . AddUser

postNamedUser :: AcidState DB -> UserId -> User -> RestAction ()
postNamedUser st uid user = liftIO $ update' st (UpdateUser uid user)

deleteUser :: AcidState DB -> UserId -> RestAction ()
deleteUser st = liftIO . update' st . DeleteUser


-- * service

type ThentosService =
       Get [ServiceId]
  :<|> Capture "name" ServiceId :> Get Service
  :<|> Post ServiceId

thentosService st =
         getServiceIds st
    :<|> getService st
    :<|> postNewService st

getServiceIds :: AcidState DB -> RestAction [ServiceId]
getServiceIds st = liftIO $ query' st AllServiceIDs

getService :: AcidState DB -> ServiceId -> RestAction Service
getService st id =
    liftIO (query' st (LookupService id)) >>= maybe noSuchService right

postNewService :: AcidState DB -> RestAction ServiceId
postNewService st = liftIO $ update' st AddService


-- * helpers

noSuchUser :: RestAction a
noSuchUser = left (404, "no such user")  -- FIXME: correct status code?

noSuchService :: RestAction a
noSuchService = left (404, "no such service")

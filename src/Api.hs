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

import Control.Lens ((%~))
import Control.Monad.State (liftIO)
import Control.Monad.Trans.Either (EitherT, right, left)
import Data.Acid (AcidState)
import Data.Acid.Advanced (query', update')
import Data.Map (Map)
import Data.String.Conversions (ST)
import Data.Thyme.Time (addDays)
import Data.Thyme (UTCTime, getCurrentTime, _utctDay)
import Servant.API ((:<|>)((:<|>)), (:>), Get, Post, Put, Delete, Capture, ReqBody)
import Servant.Server (Server)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Map as Map

import DB
import Types


type RestAction = EitherT (Int, String) IO

type App = ThentosBasic

type ThentosBasic =
       "user" :> ThentosUser
  :<|> "service" :> ThentosService
  :<|> "session" :> ThentosSession

app :: AcidState DB -> Server App
app st =
       thentosUser st
  :<|> thentosService st
  :<|> thentosSession st


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


-- * session

type ThentosSession =
       Get [SessionToken]
  :<|> Capture "token" SessionToken :> Get Session
  :<|> ReqBody (UserId, ServiceId) :> Post SessionToken

  -- the following can work with the above only if
  -- https://github.com/haskell-servant/servant/issues/3 has been
  -- fixed:
  -- :<|> ReqBody (UserId, ServiceId, UTCTime) :> Post Session

  :<|> Capture "token" SessionToken :> "logout" :> Get ()
  :<|> Capture "token" (SessionToken, ServiceId) :> "active" :> Get Bool

thentosSession st =
       getSessionTokens st
  :<|> getSession st
  :<|> createSession st
  :<|> endSession st
  :<|> isActiveSession st


getSessionTokens :: AcidState DB -> RestAction [SessionToken]
getSessionTokens st = query' st AllSessionTokens

getSession :: AcidState DB -> SessionToken -> RestAction Session
getSession st tok = query' st (LookupSession tok) >>= maybe noSuchSession right

-- | Sessions have a fixed duration of 2 weeks.
--
-- FIXME: I *think* the time stamps will be created once at the time
-- of the http request, and not be changed to current times when
-- 'ChangeLog' is replayed.  But I would still feel better if we had a
-- test for that.
createSession :: AcidState DB -> (UserId, ServiceId) -> RestAction SessionToken
createSession st (uid, sid) = do
    start :: UTCTime <- liftIO $ getCurrentTime
    let end :: UTCTime = _utctDay %~ addDays 14 $ start
    update' st $ StartSession uid sid start end

endSession :: AcidState DB -> SessionToken -> RestAction ()
endSession st = liftIO . update' st . EndSession

isActiveSession :: AcidState DB -> (SessionToken, ServiceId) -> RestAction Bool
isActiveSession st = liftIO . query' st . IsActiveSession


-- * helpers

noSuchUser :: RestAction a
noSuchUser = left (404, "no such user")  -- FIXME: correct status code?

noSuchService :: RestAction a
noSuchService = left (404, "no such service")

noSuchSession :: RestAction a
noSuchSession = left (404, "no such session")

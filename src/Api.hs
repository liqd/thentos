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
import Data.AffineSpace ((.+^))
import Data.Thyme.Time ()
import Data.Thyme (UTCTime, getCurrentTime)
import Servant.API ((:<|>)((:<|>)), (:>), Get, Post, Put, Delete, Capture, ReqBody)
import Servant.Server (Server)

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
getService st sid =
    liftIO (query' st (LookupService sid)) >>= maybe noSuchService right

postNewService :: AcidState DB -> RestAction ServiceId
postNewService st = liftIO $ update' st AddService


-- * session

type ThentosSession =
       Get [SessionToken]
  :<|> Capture "token" SessionToken :> Get Session
  :<|> ReqBody (UserId, ServiceId) :> Post SessionToken
  :<|> ReqBody (UserId, ServiceId, Timeout) :> Post SessionToken
  :<|> Capture "token" SessionToken :> "logout" :> Get ()
  :<|> Capture "sid" ServiceId :> Capture "token" SessionToken :> "active" :> Get Bool

thentosSession st =
       getSessionTokens st
  :<|> getSession st
  :<|> createSession st
  :<|> createSessionWithTimeout st
  :<|> endSession st
  :<|> isActiveSession st


getSessionTokens :: AcidState DB -> RestAction [SessionToken]
getSessionTokens st = query' st AllSessionTokens

getSession :: AcidState DB -> SessionToken -> RestAction Session
getSession st tok = query' st (LookupSession tok) >>= maybe noSuchSession right

-- | Sessions have a fixed duration of 2 weeks.
createSession :: AcidState DB -> (UserId, ServiceId) -> RestAction SessionToken
createSession st (uid, sid) = createSessionWithTimeout st (uid, sid, Timeout $ 14 * 24 * 3600)

-- | Sessions with explicit timeout.
--
-- FIXME: I *think* the time stamps will be created once at the time
-- of the http request, and not be changed to current times when
-- 'ChangeLog' is replayed.  But I would still feel better if we had a
-- test for that.
createSessionWithTimeout :: AcidState DB -> (UserId, ServiceId, Timeout) -> RestAction SessionToken
createSessionWithTimeout st (uid, sid, Timeout diff) = do
    now :: UTCTime <- liftIO getCurrentTime
    update' st $ StartSession uid sid (TimeStamp now) (TimeStamp $ now .+^ diff)

endSession :: AcidState DB -> SessionToken -> RestAction ()
endSession st = liftIO . update' st . EndSession

isActiveSession :: AcidState DB -> ServiceId -> SessionToken -> RestAction Bool
isActiveSession st sid  = liftIO . query' st . IsActiveSession sid


-- * helpers

noSuchUser :: RestAction a
noSuchUser = left (404, "no such user")  -- FIXME: correct status code?

noSuchService :: RestAction a
noSuchService = left (404, "no such service")

noSuchSession :: RestAction a
noSuchSession = left (404, "no such session")

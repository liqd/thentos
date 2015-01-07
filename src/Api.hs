{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
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

import Control.Applicative ((<$>))
import Control.Monad.State (liftIO)
import Control.Monad.Trans.Either (EitherT, right, left)
import Data.Acid (AcidState)
import Data.AffineSpace ((.+^))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs)
import Data.Text.Encoding (decodeUtf8)
import Data.Thyme.Time ()
import Data.Thyme (UTCTime, getCurrentTime)
import LIO.DCLabel (DCLabel, (%%))
import LIO (LIOState(LIOState), evalLIO)
import Network.Wai (requestHeaders)
import Servant.API ((:<|>)((:<|>)), (:>), Get, Post, Put, Delete, Capture, ReqBody)
import Servant.Docs (HasDocs, docsFor)
import Servant.Server.Internal (HasServer, Server, route)

import DB
import Types


type RestAction = EitherT (Int, String) IO

type App = ThentosAuth :> ThentosBasic

type ThentosBasic =
       "user" :> ThentosUser
  :<|> "service" :> ThentosService
  :<|> "session" :> ThentosSession

app :: AcidState DB -> Auth -> Server ThentosBasic
app st auth =
       thentosUser st auth
  :<|> thentosService st auth
  :<|> thentosSession st auth


-- * authentication

-- | Empty data type for triggering authentication.  If you have an
-- api type 'API', use like this: @ThentosAuth :> API@, then write a
-- route handler that takes 'Auth' as an extra argument.  'Auth' will
-- be parsed from the headers and injected into the @sublayout@
-- handler.
data ThentosAuth

-- | Result type of 'ThentosAuth'.  Contains authentication
-- information in the form required by @LIO@ in module "DB".
type Auth = LIOState DCLabel

-- | See 'ThentosAuth'.
instance HasServer sublayout => HasServer (ThentosAuth :> sublayout)
  where
    type Server (ThentosAuth :> sublayout) = Auth -> Server sublayout

    route Proxy subserver request respond = do
      let mprincipal :: Maybe ST = decodeUtf8 <$> lookup "X-Principal" (requestHeaders request)
          mpassword  :: Maybe ST = decodeUtf8 <$> lookup "X-Password"  (requestHeaders request)

      route (Proxy :: Proxy sublayout) (subserver $ mkAuth mprincipal mpassword) request respond

-- | FIXME: not much documentation yet.
instance HasDocs sublayout => HasDocs (ThentosAuth :> sublayout) where
  docsFor Proxy = docsFor (Proxy :: Proxy sublayout)

mkAuth :: Maybe ST -> Maybe ST -> Auth
mkAuth Nothing                              _ = LIOState (True %% True) (True %% True)
mkAuth (Just (cs -> (principal :: String))) _ = LIOState (True %% True) (principal %% principal)


-- * user

type ThentosUser =
       Get [UserId]
  :<|> Capture "userid" UserId :> Get User
  :<|> Capture "userid" UserId :> ReqBody User :> Put ()
  :<|> ReqBody User :> Post UserId
  :<|> Capture "userid" UserId :> Delete

thentosUser :: AcidState DB -> Auth -> Server ThentosUser
thentosUser st auth =
       getUserIds st auth
  :<|> getUser st auth
  :<|> postNamedUser st auth
  :<|> postNewUser st auth
  :<|> deleteUser st auth

getUserIds :: AcidState DB -> Auth -> RestAction [UserId]
getUserIds st auth = liftIO $ evalLIO (queryLIO st AllUserIDs) auth

getUser :: AcidState DB -> Auth -> UserId -> RestAction User
getUser st auth uid = liftIO (evalLIO (queryLIO st (LookupUser uid)) auth) >>= maybe noSuchUser right

postNewUser :: AcidState DB -> Auth -> User -> RestAction UserId
postNewUser st auth user = liftIO $ evalLIO (updateLIO st $ AddUser user) auth

postNamedUser :: AcidState DB -> Auth -> UserId -> User -> RestAction ()
postNamedUser st auth uid user = liftIO $ evalLIO (updateLIO st (UpdateUser uid user)) auth

deleteUser :: AcidState DB -> Auth -> UserId -> RestAction ()
deleteUser st auth uid = liftIO $ evalLIO (updateLIO st $ DeleteUser uid) auth


-- * service

type ThentosService =
       Get [ServiceId]
  :<|> Capture "sid" ServiceId :> Get Service
  :<|> Post ServiceId

thentosService :: AcidState DB -> Auth -> Server ThentosService
thentosService st auth =
         getServiceIds st auth
    :<|> getService st auth
    :<|> postNewService st auth

getServiceIds :: AcidState DB -> Auth -> RestAction [ServiceId]
getServiceIds st auth = liftIO $ evalLIO (queryLIO st AllServiceIDs) auth

getService :: AcidState DB -> Auth -> ServiceId -> RestAction Service
getService st auth sid =
    liftIO (evalLIO (queryLIO st (LookupService sid)) auth) >>= maybe noSuchService right

postNewService :: AcidState DB -> Auth -> RestAction ServiceId
postNewService st auth = liftIO $ evalLIO (updateLIO st AddService) auth


-- * session

type ThentosSession =
       Get [SessionToken]
  :<|> Capture "token" SessionToken :> Get Session
  :<|> ReqBody (UserId, ServiceId) :> Post SessionToken
  :<|> ReqBody (UserId, ServiceId, Timeout) :> Post SessionToken
  :<|> Capture "token" SessionToken :> "logout" :> Get ()
  :<|> Capture "sid" ServiceId :> Capture "token" SessionToken :> "active" :> Get Bool

thentosSession :: AcidState DB -> Auth -> Server ThentosSession
thentosSession st auth =
       getSessionTokens st auth
  :<|> getSession st auth
  :<|> createSession st auth
  :<|> createSessionWithTimeout st auth
  :<|> endSession st auth
  :<|> isActiveSession st auth


getSessionTokens :: AcidState DB -> Auth -> RestAction [SessionToken]
getSessionTokens st auth = liftIO $ evalLIO (queryLIO st AllSessionTokens) auth

getSession :: AcidState DB -> Auth -> SessionToken -> RestAction Session
getSession st auth tok = liftIO (evalLIO (queryLIO st (LookupSession tok)) auth) >>= maybe noSuchSession right

-- | Sessions have a fixed duration of 2 weeks.
createSession :: AcidState DB -> Auth -> (UserId, ServiceId) -> RestAction SessionToken
createSession st auth (uid, sid) = createSessionWithTimeout st auth (uid, sid, Timeout $ 14 * 24 * 3600)

-- | Sessions with explicit timeout.
--
-- FIXME: I *think* the time stamps will be created once at the time
-- of the http request, and not be changed to current times when
-- 'ChangeLog' is replayed.  But I would still feel better if we had a
-- test for that.
createSessionWithTimeout :: AcidState DB -> Auth -> (UserId, ServiceId, Timeout) -> RestAction SessionToken
createSessionWithTimeout st auth (uid, sid, Timeout diff) = liftIO $ do
    now :: UTCTime <- liftIO getCurrentTime
    evalLIO (updateLIO st $ StartSession uid sid (TimeStamp now) (TimeStamp $ now .+^ diff)) auth

endSession :: AcidState DB -> Auth -> SessionToken -> RestAction ()
endSession st auth tok = liftIO $ evalLIO (updateLIO st $ EndSession tok) auth

isActiveSession :: AcidState DB -> Auth -> ServiceId -> SessionToken -> RestAction Bool
isActiveSession st auth sid tok = liftIO $ evalLIO (queryLIO st $ IsActiveSession sid tok) auth


-- * helpers

noSuchUser :: RestAction a
noSuchUser = left (404, "no such user")  -- FIXME: correct status code?

noSuchService :: RestAction a
noSuchService = left (404, "no such service")

noSuchSession :: RestAction a
noSuchSession = left (404, "no such session")

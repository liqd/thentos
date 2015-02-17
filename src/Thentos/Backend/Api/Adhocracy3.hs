{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE DeriveGeneric                            #-}
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

{-# OPTIONS  #-}

-- | This is an implementation of
-- git@github.com:liqd/adhocracy3.git:/docs/source/api/authentication_api.rst
module Thentos.Backend.Api.Adhocracy3 where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Concurrent.MVar (MVar)
import Control.Exception (assert)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, left)
import Control.Monad (when, unless, mzero)
import Crypto.Random (SystemRNG)
import Data.Aeson (Value(Object), ToJSON, FromJSON, (.:), (.:?), (.=), object, withObject)
import Data.Functor.Infix ((<$$>))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Network.Wai (Request, Response, ResponseReceived, Application)
import Safe (readMay)
import Servant.API ((:<|>)((:<|>)), (:>), Get, Post, Put, Delete, Capture, ReqBody)
import Servant.Docs (HasDocs, docsFor, docs, markdown)
import Servant.Server.Internal (HasServer, Server, route)
import Servant.Server (serve)
import Text.Printf (printf)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as ST

import Thentos.Api
import Thentos.Backend.Api.Proxy
import Thentos.Backend.Core (RestAction, RestActionState, PushActionC, PushActionSubRoute, pushAction, lookupRequestHeader)
import Thentos.DB
import Thentos.Doc ()
import Thentos.Types
import Thentos.Util


-- * data types

-- ** basics

newtype Path = Path ST
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance ToJSON Path
instance FromJSON Path

data ContentType =
      CTUser
  deriving (Eq, Ord, Enum, Bounded, Typeable, Generic)

instance Show ContentType where
    show CTUser = "adhocracy_core.resources.principal.IUser"

instance Read ContentType where
    readsPrec = readsPrecEnumBoundedShow

instance ToJSON ContentType
instance FromJSON ContentType

data PropertySheet =
      PSUserBasic
    | PSPasswordAuthentication
  deriving (Eq, Enum, Bounded, Typeable)

instance Show PropertySheet where
    show PSUserBasic              = "adhocracy_core.sheets.principal.IUserBasic"
    show PSPasswordAuthentication = "adhocracy_core.sheets.principal.IPasswordAuthentication"

instance Read PropertySheet where
    readsPrec = readsPrecEnumBoundedShow


-- ** resource

data A3Resource a = A3Resource (Maybe Path) (Maybe ContentType) (Maybe a)
  deriving (Eq, Show, Typeable, Generic)

instance ToJSON a => ToJSON (A3Resource a) where
    toJSON (A3Resource p ct r) =
        case Aeson.toJSON r of
            Object v -> object $ "path" .= p : "content_type" .= ct : HashMap.toList v

instance FromJSON a => FromJSON (A3Resource a) where
    parseJSON = withObject "resource object" $ \ v -> do
        A3Resource <$> (v .:? "path") <*> (v .:? "content_type") <*> Aeson.parseJSON (Object v)


-- ** individual resources

newtype A3User = A3User { fromA3User :: UserFormData }
  deriving (Eq, Show, Typeable, Generic)

instance ToJSON A3User where
    toJSON (A3User (UserFormData name _ email)) = object
        [ "content_type" .= CTUser
        , "data" .= object
            [ cshow PSUserBasic .= object
                [ "name" .= name
                , "email" .= email
                ]
            ]
        ]

instance FromJSON A3User where
    parseJSON = withObject "resource object" $ \ v -> do
        content_type :: ContentType <- v .: "content_type"
        when (content_type /= CTUser) $
            fail $ "wrong content type: " ++ show content_type
        name         <- v .: "data" >>= (.: cshow PSUserBasic) >>= (.: "name")
        password     <- v .: "data" >>= (.: cshow PSPasswordAuthentication) >>= (.: "password")
        email        <- v .: "data" >>= (.: cshow PSUserBasic) >>= (.: "email")
        when (not $ userNameValid name) $
            fail $ "malformed user name: " ++ show name
        when (not $ emailValid name) $
            fail $ "malformed email address: " ++ show email
        when (not $ passwordGood name) $
            fail $ "bad password: " ++ show password
        return . A3User $ UserFormData (UserName name) (UserPass password) (UserEmail email)

-- | constraints on user name: The "name" field in the "IUserBasic"
-- schema is a non-empty string that can contain any characters except
-- '@' (to make user names distinguishable from email addresses). The
-- username must not contain any whitespace except single spaces,
-- preceded and followed by non-whitespace (no whitespace at begin or
-- end, multiple subsequent spaces are forbidden, tabs and newlines
-- are forbidden).
--
-- FIXME: not implemented.
userNameValid :: ST -> Bool
userNameValid _ = True

-- | RFC 5322 (sections 3.2.3 and 3.4.1) and RFC 5321
--
-- FIXME: not implemented.
emailValid :: ST -> Bool
emailValid _ = True

-- | Only an empty password is a bad password.
passwordGood :: ST -> Bool
passwordGood "" = False
passwordGood _ = True


-- ** other types

data ActivationRequest =
    ActivationRequest Path
  deriving (Eq, Show, Typeable, Generic)

data LoginRequest =
    LoginByName UserName UserPass
  | LoginByEmail UserEmail UserPass
  deriving (Eq, Show, Typeable, Generic)

data RequestResult =
    RequestSuccess Path SessionToken
  | RequestError [ST]
  deriving (Eq, Show, Typeable, Generic)

instance FromJSON ActivationRequest where
    parseJSON = withObject "activation request" $ \ v -> do
        p :: ST <- v .: "path"
        unless ("/activate/" `ST.isPrefixOf` p) $
            fail $ "ActivationRequest with malformed path: " ++ show p
        return . ActivationRequest . Path $ p

instance FromJSON LoginRequest where
    parseJSON = withObject "login request" $ \ v -> do
        n <- UserName  <$$> v .:? "name"
        e <- UserEmail <$$> v .:? "email"
        p <- UserPass  <$>  v .: "password"
        case (n, e) of
          (Just x,  Nothing) -> return $ LoginByName x p
          (Nothing, Just x)  -> return $ LoginByEmail x p
          (_,       _)       -> fail $ "malformed login request body: " ++ show v

instance ToJSON RequestResult where
    toJSON (RequestSuccess p t) = object $
        "status" .= ("success" :: String) :
        "user_path" .= p :
        "token" .= t :
        []
    toJSON (RequestError es) = object $
        "status" .= ("error" :: String) :
        "errors" .= map (\ d -> object ["description" .= d, "location" .= (), "name" .= ()]) es :
        []


-- * main

runBackend :: Int -> ActionStateGlobal (MVar SystemRNG) -> IO ()
runBackend port = run port . serveApi

serveApi :: ActionStateGlobal (MVar SystemRNG) -> Application
serveApi = serve (Proxy :: Proxy App) . app

-- apiDocs :: String
-- apiDocs = markdown $ docs (Proxy :: Proxy App)


-- * api

-- | Note: login_username and login_email have identical behavior.  In
-- particular, it is not an error to send username and password to
-- @/login_email@.  This makes implementing all sides of the protocol
-- a lot easier without sacrificing security.
type App =
       "principals" :> "users" :> ReqBody A3User :> Post (A3Resource ())  -- FIXME: this will probably crash because '()' is not a json object.
  :<|> "activate_account"      :> ReqBody ActivationRequest :> Post RequestResult
  :<|> "login_username"        :> ReqBody LoginRequest :> Post RequestResult
  :<|> "login_email"           :> ReqBody LoginRequest :> Post RequestResult
  :<|> ServiceProxy

app :: ActionStateGlobal (MVar SystemRNG) -> Server App
app asg = p $
       addUser
  :<|> activate
  :<|> login
  :<|> login
  :<|> serviceProxy
  where
    p = pushAction (asg, \ _ _ -> Right allowEverything)  -- FIXME: this is kinda bad security.


-- * handler

addUser :: A3User -> RestAction (A3Resource ())
addUser (A3User user) = do
    (uid :: UserId, _ :: ConfirmationToken) <- addUnconfirmedUser user

    -- FIXME: send confirmation email

    return $ A3Resource (Just $ userIdToPath uid) (Just CTUser) Nothing

    -- FIXME: catch errors and respond with RequestError


activate :: ActivationRequest -> RestAction RequestResult
activate (ActivationRequest p) = do
    ctok :: ConfirmationToken <- confirmationTokenFromPath p
    uid  :: UserId            <- updateAction $ FinishUserRegistration ctok
    stok :: SessionToken      <- startSessionNow (UserA uid)
    return $ RequestSuccess (userIdToPath uid) stok

    -- FIXME: catch errors and respond with RequestError


-- | FIXME: check password!
login :: LoginRequest -> RestAction RequestResult
login (LoginByName uname _) = do
    (uid, _)             <- queryAction $ LookupUserByName uname
    stok :: SessionToken <- startSessionNow (UserA uid)
    return $ RequestSuccess (userIdToPath uid) stok
login (LoginByEmail uemail _) = do
    (uid, _)             <- queryAction $ LookupUserByEmail uemail
    stok :: SessionToken <- startSessionNow (UserA uid)
    return $ RequestSuccess (userIdToPath uid) stok


-- * aux

userIdToPath :: UserId -> Path
userIdToPath (UserId i) = Path . cs $ (printf "/princicpals/users/%7.7i" i :: String)

userIdFromPath :: Path -> RestAction UserId
userIdFromPath (Path s) = maybe (lift . left $ NoSuchUser) return $
    case ST.splitAt (ST.length prefix) s of
        (prefix', s') | prefix' == prefix -> fmap UserId . readMay . cs $ s'
        _ -> Nothing
  where
    prefix = "/principals/users/"

confirmationTokenFromPath :: Path -> RestAction ConfirmationToken
confirmationTokenFromPath = assert False $ error "confirmationTokenFromPath"

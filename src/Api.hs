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
{-# LANGUAGE TypeSynonymInstances                     #-}

{-# OPTIONS -fno-warn-unused-imports -fwarn-incomplete-patterns -fdefer-type-errors #-}

module Api
where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
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
import Rest
import Rest.Api
import Safe
import System.Environment

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Map as Map
import qualified Rest.Resource as Rest

import DB
import Types


newtype App a = App { unApp :: ReaderT (AcidState DB) IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (AcidState DB))


runApi :: AcidState DB -> App a -> IO a
runApi s (App a) = a `runReaderT` s

api :: Api App
api = [(mkVersion 0 0 1, Some1 router)]

router :: Router App App
router = root -/ route user


user :: Resource App (ReaderT String App) String () Void
user = mkResourceReader
  { Rest.name = "user"
  , Rest.schema = withListing () $ named [("id", singleBy id)]
  , Rest.list = const listUser
  , Rest.get = Just getUser
  , Rest.create = Just createUser
  , Rest.update = Just updateUser
  }

listUser :: ListHandler App
listUser = mkListing (jsonO . someO) $ \ _ -> ask >>= \ st -> query' st AllUsers

getUser :: Handler (ReaderT String App)
getUser = mkIdHandler (jsonO . someO) $ \ _ idString -> _parseUserID idString >>= \ idInt -> _getUser idInt

_parseUserID :: String -> ErrorT (Reason ()) (ReaderT String App) UserID
_parseUserID userIdString = maybe e return $ readMay userIdString
  where e = throwError . IdentError . ParseError $ "could not parse user id: " <> show userIdString

_getUser :: UserID -> ErrorT (Reason ()) (ReaderT String App) User
_getUser userId = do
    result :: Maybe User <- lift . lift $ ask >>= \ st -> query' st $ LookupUser userId
    maybe e return result
  where e = throwError NotFound


-- | Accept a 'User' without user id and store it under a fresh user
-- id in the database.  Respond with fresh user id on success.  If
-- user id is non-empty in request, respond with an error.
createUser :: Handler App
createUser = mkHandler (jsonI . someI . jsonO . someO) $ _upd . input
  where
    _upd :: User -> ErrorT (Reason ()) App UserID
    _upd user = if isNothing $ user ^. userID
      then lift $ ask >>= \ st -> update' st $ InsertUser user
      else throwError . InputError . UnsupportedFormat $ "user id field not allowed in POST requests."


-- | Accept a 'User' under a user id path and update it in the
-- database.  If user id differ in body and request path, throw an
-- error.
updateUser :: Handler (ReaderT String App)
updateUser = mkIdHandler (jsonI . someI . jsonO . someO) $ \ user idString -> _parseUserID idString >>= _upd user
  where
    _upd :: User -> UserID -> ErrorT (Reason ()) (ReaderT String App) UserID
    _upd user uid = if user ^. userID `elem` [Nothing, Just uid]
      then lift . lift $ ask >>= \ st -> update' st $ InsertUser user
      else throwError . InputError . UnsupportedFormat $ "user id differ in body and request path."

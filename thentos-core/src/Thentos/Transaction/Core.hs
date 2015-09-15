{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Thentos.Transaction.Core
    ( ThentosQuery
    , Defaultable(..)
    , runThentosQuery
    , queryT
    , execT
    , createDB
    , schemaFile
    , wipeFile
    , catchViolation
    , catcher
    , orDefault
    )
where

import Control.Monad.Except (throwError)
import Control.Exception.Lifted (catch, throwIO)
import Control.Monad (void, liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Int (Int64)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Connection, SqlError, ToRow, FromRow, Query, query, execute,
    execute_)
import Database.PostgreSQL.Simple.Errors (constraintViolation,
    ConstraintViolation(ForeignKeyViolation, UniqueViolation))
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Database.PostgreSQL.Simple.Transaction (withTransaction)
import Database.PostgreSQL.Simple.Types (Default(Default))

import Paths_thentos_core
import Thentos.Types

type ThentosQuery e a = EitherT (ThentosError e) (ReaderT Connection IO) a

schemaFile :: IO FilePath
schemaFile = getDataFileName "schema/schema.sql"

wipeFile :: IO FilePath
wipeFile = getDataFileName "schema/wipe.sql"

-- | Creates the database schema if it does not already exist.
createDB :: Connection -> IO ()
createDB conn = do
    schema <- readFile =<< schemaFile
    void $ execute_ conn (fromString schema)

-- | Execute a 'ThentosQuery'. Every query is a DB transaction, so the DB state won't change
-- if there are any errors, nor will other queries encouter a possibly inconsistent interim state.
runThentosQuery :: Connection -> ThentosQuery e a -> IO (Either (ThentosError e) a)
runThentosQuery conn q = do
    withTransaction conn $ runReaderT (runEitherT q) conn

queryT :: (ToRow q, FromRow r) => Query -> q -> ThentosQuery e [r]
queryT q x = do
    conn <- ask
    e <- catchViolation catcher . liftIO . liftM Right $ query conn q x
    either throwError return e

execT :: ToRow q => Query -> q -> ThentosQuery e Int64
execT q x = do
    conn <- ask
    e <- catchViolation catcher . liftIO . liftM Right $ execute conn q x
    either throwError return e

-- | Convert known SQL constraint errors to 'ThentosError', rethrowing unknown
-- ones.
catcher :: MonadBaseControl IO m => SqlError -> ConstraintViolation -> m (Either (ThentosError e) a)
catcher _ (UniqueViolation "users_pkey")      = return $ Left UserIdAlreadyExists
catcher _ (UniqueViolation "users_name_key")  = return $ Left UserNameAlreadyExists
catcher _ (UniqueViolation "users_email_key") = return $ Left UserEmailAlreadyExists
catcher _ (UniqueViolation "user_confirmation_tokens_token_key")
    = return $ Left ConfirmationTokenAlreadyExists
catcher _ (ForeignKeyViolation "user_sessions" "user_sessions_uid_fkey") = return $ Left NoSuchUser
catcher e _                                   = throwIO e

-- | Like @postgresql-simple@'s 'catchViolation', but generalized to
-- @MonadBaseControl IO m@
catchViolation :: MonadBaseControl IO m => (SqlError -> ConstraintViolation -> m a) -> m a -> m a
catchViolation f m = m `catch` (\e -> maybe (throwIO e) (f e) $ constraintViolation e)

-- Wrap a value that may have a default value. 'Defaultable a' is structurally equivalent to
-- 'Maybe a', but postgresql-simple will convert 'Nothing' to "NULL", while we convert
-- 'DefaultVal' to "DEFAULT". This allows using default values specified by the DB schema,
-- e.g. auto-incrementing sequences.
data Defaultable a = DefaultVal | CustomVal a
    deriving (Eq, Ord, Read, Show)

instance (ToField a) => ToField (Defaultable a) where
    toField DefaultVal    = toField Default
    toField (CustomVal a) = toField a

-- Convert a 'Maybe' into a 'Defaultable' instance.
orDefault :: ToField a => Maybe a -> Defaultable a
orDefault = maybe DefaultVal CustomVal

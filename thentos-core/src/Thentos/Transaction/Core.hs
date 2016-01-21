{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Control.Exception.Lifted (catch, throwIO)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Control.Monad (void, liftM)
import Data.Int (Int64)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Connection, SqlError, ToRow, FromRow, Query, query, execute,
    execute_)
import Database.PostgreSQL.Simple.Errors (constraintViolation,
    ConstraintViolation(ForeignKeyViolation, UniqueViolation))
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Database.PostgreSQL.Simple.Transaction (withTransaction)
import Database.PostgreSQL.Simple.Types (Default(Default))

import Thentos.Types


type ThentosQuery e a = EitherT (ThentosError e) (ReaderT Connection IO) a

schemaFile :: FilePath
schemaFile = "./schema/schema.sql"

wipeFile :: FilePath
wipeFile = "./schema/wipe.sql"

-- | Creates the database schema if it does not already exist.
createDB :: Connection -> IO ()
createDB conn = do
    schema <- readFile schemaFile
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
catcher e = f
  where
    r = return . Left

    f (UniqueViolation "users_pkey") = r UserIdAlreadyExists
    f (UniqueViolation "users_name_key") = r UserNameAlreadyExists
    f (UniqueViolation "users_email_key") = r UserEmailAlreadyExists
    f (UniqueViolation "personas_name_key") = r PersonaNameAlreadyExists
    f (UniqueViolation "contexts_owner_service_name_key") = r ContextNameAlreadyExists
    f (UniqueViolation "user_confirmation_tokens_token_key") = r ConfirmationTokenAlreadyExists
    f (UniqueViolation "captchas_pkey") = r CaptchaIdAlreadyExists
    f (ForeignKeyViolation "personas" "personas_uid_fkey") = r NoSuchUser
    f (ForeignKeyViolation "contexts" "contexts_owner_service_fkey") = r NoSuchService
    f (ForeignKeyViolation "thentos_sessions" "thentos_sessions_uid_fkey") = r NoSuchUser
    f (ForeignKeyViolation "thentos_sessions" "thentos_sessions_sid_fkey") = r NoSuchService
    f (ForeignKeyViolation "personas_per_context" "personas_per_context_persona_id_fkey") =
        r NoSuchPersona
    f (ForeignKeyViolation "personas_per_context" "personas_per_context_context_id_fkey") =
        r NoSuchContext
    f _ = throwIO e

-- | Like @postgresql-simple@'s 'catchViolation', but generalized to
-- @MonadBaseControl IO m@
catchViolation :: MonadBaseControl IO m => (SqlError -> ConstraintViolation -> m a) -> m a -> m a
catchViolation f m = m `catch` (\e -> maybe (throwIO e) (f e) $ constraintViolation e)

-- | Wrap a value that may have a default value. 'Defaultable a' is structurally equivalent to
-- 'Maybe a', but postgresql-simple will convert 'Nothing' to "NULL", while we convert
-- 'DefaultVal' to "DEFAULT". This allows using default values specified by the DB schema,
-- e.g. auto-incrementing sequences.
data Defaultable a = DefaultVal | CustomVal a
    deriving (Eq, Ord, Read, Show)

instance (ToField a) => ToField (Defaultable a) where
    toField DefaultVal    = toField Default
    toField (CustomVal a) = toField a

-- | Convert a 'Maybe' into a 'Defaultable' instance.
orDefault :: ToField a => Maybe a -> Defaultable a
orDefault = maybe DefaultVal CustomVal

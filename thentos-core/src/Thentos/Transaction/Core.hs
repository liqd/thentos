{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Thentos.Transaction.Core
    ( ThentosQuery
    , runThentosQuery
    , queryT
    , execT
    , createDB
    , schemaFile
    )
where

import Control.Monad.Except (throwError)
import Control.Exception.Lifted (catch, throwIO)
import Control.Monad (void, mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Connection, SqlError, ToRow, FromRow, Query, query, execute, execute_, sqlExecStatus)
import Database.PostgreSQL.Simple.Errors (constraintViolation, ConstraintViolation(UniqueViolation))
import Paths_thentos_core

import Thentos.Types

type ThentosQuery a = EitherT ThentosError (ReaderT Connection IO) a

schemaFile :: IO FilePath
schemaFile = getDataFileName "schema/schema.sql"

-- | Creates the database schema if it does not already exist.
createDB :: Connection -> IO ()
createDB conn = do
    schema <- readFile =<< schemaFile
    void $ execute_ conn (fromString schema)

runThentosQuery :: Connection -> ThentosQuery a -> IO (Either ThentosError a)
runThentosQuery conn = flip runReaderT conn . runEitherT

queryT :: (ToRow q, FromRow r) => Query -> q -> ThentosQuery [r]
queryT q x = do
    conn <- ask
    liftIO $ query conn q x

execT :: ToRow q => Query -> q -> ThentosQuery ()
execT q x = do
    conn <- ask
    e <- catchViolation catcher . liftIO $ execute conn q x >> return Nothing
    case e of
        Just err -> throwError err
        Nothing -> return ()

-- | Convert known SQL constraint errors to 'ThentosError', rethrowing unknown
-- ones.
catcher :: MonadBaseControl IO m => SqlError -> ConstraintViolation -> m (Maybe ThentosError)
catcher _ (UniqueViolation "users_id_key") = return $ Just UserIdAlreadyExists
catcher _ (UniqueViolation "users_name_key") = return $ Just UserNameAlreadyExists
catcher e _                                = throwIO e

-- | Like @postgresql-simple@'s 'catchViolation', but generalized to
-- @MonadBaseControl IO m@
catchViolation :: MonadBaseControl IO m => (SqlError -> ConstraintViolation -> m a) -> m a -> m a
catchViolation f m = m `catch` (\e -> maybe (throwIO e) (f e) $ constraintViolation e)

module Thentos.Transaction.Core
    ( ThentosQuery
    , ThentosUpdate
    , runThentosQuery
    , runThentosUpdate
    , queryT
    , execT
    , createDB
    , schemaFile
    )
where

import Control.Exception (catch)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Connection, ToRow, FromRow, Query, query, execute, execute_, sqlExecStatus)
import Paths_thentos_core

import Thentos.Types

type ThentosQuery  a = EitherT ThentosError (ReaderT Connection IO) a
type ThentosUpdate a = EitherT ThentosError (ReaderT Connection IO) a

schemaFile :: IO FilePath
schemaFile = getDataFileName "schema/schema.sql"

-- | Creates the database schema if it does not already exist.
createDB :: Connection -> IO ()
createDB conn = do
    schema <- readFile =<< schemaFile
    void $ execute_ conn (fromString schema)

runThentosUpdate :: Connection -> ThentosUpdate a -> IO (Either ThentosError a)
runThentosUpdate conn = flip runReaderT conn . runEitherT

runThentosQuery :: Connection -> ThentosQuery a -> IO (Either ThentosError a)
runThentosQuery = runThentosUpdate

queryT :: (ToRow q, FromRow r) => Query -> q -> ThentosQuery [r]
queryT q x = do
    conn <- ask
    liftIO $ query conn q x

execT :: ToRow q => Query -> q -> ThentosUpdate ()
execT q x = do
    conn <- ask
    void $ liftIO $ execute conn q x

module Thentos.Transaction.Core
    ( ThentosQuery
    , ThentosUpdate
    , runThentosQuery
    , runThentosUpdate
    , queryT
    , execT
    )
where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Database.PostgreSQL.Simple (Connection, ToRow, FromRow, Query, query, execute)

import Thentos.Types

type ThentosQuery  a = EitherT ThentosError (ReaderT Connection IO) a
type ThentosUpdate a = EitherT ThentosError (ReaderT Connection IO) a

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

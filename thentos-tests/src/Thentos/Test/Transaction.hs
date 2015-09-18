module Thentos.Test.Transaction where

import Data.Pool (Pool, withResource)
import Data.Void (Void)
import Database.PostgreSQL.Simple (Connection, FromRow, ToRow, Query, query, query_)

import Thentos.Transaction.Core
import Thentos.Types

-- | Like 'runThentosQuery', but take connection from pool.
runThentosQueryFromPool :: Pool Connection -> ThentosQuery e a -> IO (Either (ThentosError e) a)
runThentosQueryFromPool connPool q = withResource connPool $ \conn -> runThentosQuery conn q

-- | Like 'runThentosQueryFromPool', but specialize error type to Void.
runQuery :: Pool Connection -> ThentosQuery Void a -> IO (Either (ThentosError Void) a)
runQuery = runThentosQueryFromPool

-- | Take a connection from the pool and execute the query.
doQuery :: (ToRow q, FromRow r) => Pool Connection -> Query -> q -> IO [r]
doQuery connPool stmt params = withResource connPool $ \conn -> query conn stmt params

doQuery_ :: FromRow r => Pool Connection -> Query -> IO [r]
doQuery_ connPool stmt = withResource connPool $ \conn -> query_ conn stmt


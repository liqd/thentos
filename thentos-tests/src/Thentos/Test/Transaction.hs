{-# LANGUAGE OverloadedStrings #-}

module Thentos.Test.Transaction where

import Data.Monoid ((<>))
import Data.Pool (Pool, withResource)
import Data.Void (Void)
import Data.String.Conversions (SBS)
import Database.PostgreSQL.Simple (Connection, FromRow, ToRow, Only(..), query, query_)
import Database.PostgreSQL.Simple.Types (Query(..))
import Test.Hspec (shouldBe)

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

-- | Check that a database table contains the expected number of rows.
-- DON'T use this in production case, it's totally unprotected against SQL injection!
rowCountShouldBe :: Pool Connection -> SBS -> Int -> IO ()
rowCountShouldBe connPool table count = do
    [Only actualCount] <- doQuery_ connPool . Query $ "SELECT COUNT(*) FROM " <> table
    actualCount `shouldBe` count

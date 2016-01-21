{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}

module Thentos.Test.Transaction where

import Data.Int (Int64)
import Data.Monoid ((<>))
import Data.Pool (Pool, withResource)
import Data.Void (Void)
import Data.String.Conversions (SBS)
import Database.PostgreSQL.Simple (Connection, FromRow, ToRow, Only(..), query, query_, execute)
import Database.PostgreSQL.Simple.Types (Query(..))
import GHC.Stack (CallStack)
import Test.Hspec (shouldBe)

import Thentos.Transaction.Core
import Thentos.Types

-- | Like 'runThentosQuery', but specialize error type to Void.
runVoidedQuery :: Pool Connection -> ThentosQuery Void a -> IO (Either (ThentosError Void) a)
runVoidedQuery = runThentosQuery

-- | Take a connection from the pool and execute the query.
doQuery :: (ToRow q, FromRow r) => Pool Connection -> Query -> q -> IO [r]
doQuery connPool stmt params = withResource connPool $ \conn -> query conn stmt params

doQuery_ :: FromRow r => Pool Connection -> Query -> IO [r]
doQuery_ connPool stmt = withResource connPool $ \conn -> query_ conn stmt

doTransaction :: ToRow q => Pool Connection -> Query -> q -> IO Int64
doTransaction connPool stmt params = withResource connPool $ \conn -> execute conn stmt params

-- | Check that a database table contains the expected number of rows.
-- DON'T use this in production case, it's totally unprotected against SQL injection!
rowCountShouldBe :: (?loc :: CallStack) => Pool Connection -> SBS -> Int -> IO ()
rowCountShouldBe connPool table count = do
    [Only actualCount] <- doQuery_ connPool . Query $ "SELECT COUNT(*) FROM " <> table
    actualCount `shouldBe` count

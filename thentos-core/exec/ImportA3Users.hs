{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

{- OPTIONS_GHC -Wall -Werror #-}

{- | Script for importing users from Adhocracy3.

Reads users from a file and writes them to a thentos db.  CSV must be of the form:

>>> Username;Email;Creation date;Password
>>> god;sysadmin@test.de;2015-12-08_11:30:53;$2a$10$qcUkE9u.eHexX6FY3LoBDemNxapyvv3yMFLBqExlOAvpDaYmzSrwK
>>> ...

The header line is ignored.

Creation times are converted to the current time zone of the system running the import (see main
function if you need to change that).

Thentos db should be fresh.  If any users already exist, they are ignored.  If clashes between names
or email addresses occur, the script will crash.  The creation of all users is one large DB
transaction, i.e. in case of crash, no users are created at all.


MISSING FEATURES (READ CAREFULLY BEFORE USING THIS SCRIPT!)

- Tests.  This has never been proved useable in production.  In particular, we need to confirm that
  the interplay between the users on a3 who become personas implicitly, and the users as imported into
  thentos, works as expected.

- pending confirmations: it is assume that all users in the import csv file are confirmed.  if you
  switch over from a high-volume life system, you should double-check this, and decide if it is ok to
  lose users with confirmations pending at the time of the switch-over.

SEE ALSO:
https://github.com/liqd/adhocracy3/blob/8826073bdd1de56a43406bd59f602c2c066fcddd/src/adhocracy_mercator/adhocracy_mercator/scripts/export_users.py

-}
module Main where

import Control.Exception (throwIO, ErrorCall(ErrorCall))
import Database.PostgreSQL.Simple (Only(Only), connectPostgreSQL)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.Char (ord)
import Data.Csv (FromRecord, HasHeader(..), DecodeOptions(..), parseRecord, parseField, decodeWith)
import Data.String.Conversions (cs, (<>), SBS)
import Data.Time (LocalTime, TimeZone, parseTimeM, defaultTimeLocale, localTimeToUTC, getCurrentTimeZone)
import Data.Void (Void)
import System.Environment (getArgs, getProgName)
import Thentos.Transaction.Core (ThentosQuery, runThentosQuery', queryT)
import Thentos.Types (UserName(..), UserEmail(..), HashedSecret(..), UserPass(..), UserId(..))

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Vector as Vector


data A3User = A3User !UserName !UserEmail !LocalTime !(HashedSecret UserPass)
  deriving (Show)

-- | Example: "2015-12-08_11:30:53"
parseA3Time :: Monad m => SBS -> m LocalTime
parseA3Time t = chk . parseTimeM True defaultTimeLocale fmt . cs $ t
    where
      chk = maybe (fail $ "bad timestamp: " ++ show t) return
      fmt = "%F_%T"

instance FromRecord A3User where
    parseRecord (Vector.toList -> [n, e, t, p]) = do
        e' <- parseField e
        t' <- parseA3Time t
        let n' = UserName $ cs n
            p' = BCryptHash $ cs p
        return $ A3User n' e' t' p'
    parseRecord bad = fail $ show bad

readA3User :: FilePath -> IO [A3User]
readA3User path = do
    s <- LBS.readFile path
    case decodeWith opts HasHeader s of
        Right (Vector.toList -> v) -> do
            putStrLn $ "loaded " ++ show (length v) ++ " records."
            return v
        Left e -> throwIO $ ErrorCall e
  where
    opts = DecodeOptions (fromIntegral $ ord ';')

writeA3User :: TimeZone -> String -> [A3User] -> IO ()
writeA3User tzone dbName users = do
    conn <- connectPostgreSQL . cs $ "dbname=" <> dbName
    result <- runThentosQuery' conn . sequence $ addA3User tzone <$> users
    case result of
        Right uids -> putStrLn $ "created " ++ show (length uids) ++ " new users."
        Left e -> throwIO . ErrorCall $ show e

addA3User :: TimeZone -> A3User -> ThentosQuery Void UserId
addA3User tzone (A3User n e c p) = do
    res <- queryT [sql| INSERT INTO users (name, email, created, password, confirmed)
                        VALUES (?, ?, ?, ?, ?)
                        RETURNING id |]
            (n, e, localTimeToUTC tzone c, p, True)
    case res of
        [Only uid] -> return uid
        bad -> error $ "addA3User: impossible: " ++ show bad

main :: IO ()
main = do
    prog <- getProgName
    args <- getArgs
    case args of
        [f, d] -> main' f d
        _ -> throwIO . ErrorCall $ "usage: " ++ prog ++ " <file name> <db name>"

main' :: String -> String -> IO ()
main' fileName dbName = do
    tzone <- getCurrentTimeZone
    putStrLn $ "loading from file " ++ show fileName ++ " into db " ++ show dbName ++ "."
    putStrLn $ "assuming timezone: " ++ show tzone
    readA3User fileName >>= writeA3User tzone dbName

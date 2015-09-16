module Database.PostgreSQL.Simple.Missing where

import Control.Applicative ((<*), (*>))
import Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char, decimal, digit, isDigit, peekChar, signed, takeWhile1)
import Data.Bits ((.&.))
import qualified Data.ByteString.Char8 as B8
import Data.Char (ord)
import Data.Fixed (Pico,Fixed(MkFixed))
import Data.Int (Int64)
import Data.Time (NominalDiffTime)

-- | Parse a limited postgres interval.
nominalDiffTime :: Parser NominalDiffTime
nominalDiffTime = do
    (h, m, s) <- interval
    return . fromRational . toRational $ s + 60 * fromIntegral m + 60 * 60 * fromIntegral h

-- | Parse a limited postgres interval of the form [-]HHH:MM:SS.[SSSS] (no larger units than hours).
interval :: Parser (Int, Int, Pico)
interval = do
    h <- signed decimal <* char ':'
    m <- twoDigits <* char ':'
    s <- seconds
    if m < 60 && s <= 60
        then return (h, m, s)
        else fail "invalid interval"

-- helpers seconds and twoDigits below are lifted from postgresql-simple

data T = T {-# UNPACK #-} !Int {-# UNPACK #-} !Int64

-- | Parse a count of seconds, with the integer part being two digits
-- long.
seconds :: Parser Pico
seconds = do
    real <- twoDigits
    mc <- peekChar
    case mc of
        Just '.' -> do
            t <- anyChar *> takeWhile1 isDigit
            return $! parsePicos real t
        _        -> return $! fromIntegral real
  where
    parsePicos a0 t = MkFixed (fromIntegral (t' * 10^n))
      where
        T n t'  = B8.foldl' step (T 12 (fromIntegral a0)) t
        step ma@(T m a) c
            | m <= 0    = ma
            | otherwise = T (m-1) (10 * a + fromIntegral (ord c) .&. 15)

-- | Parse a two-digit integer (e.g. day of month, hour).
twoDigits :: Parser Int
twoDigits = do
    a <- digit
    b <- digit
    let c2d c = ord c .&. 15
    return $! c2d a * 10 + c2d b

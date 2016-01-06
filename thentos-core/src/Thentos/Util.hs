{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Thentos.Util
    ( hashUserPass
    , hashServiceKey
    , secretMatches
    , verifyPass
    , verifyKey
    , makeUserFromFormData
    , mailEncode
    , cshow
    , readsPrecEnumBoundedShow
    , fmapLM
    , fmapLTM
) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Either (EitherT(EitherT), runEitherT)
import Data.String.Conversions (ConvertibleStrings, SBS, ST, cs)
import Data.List
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (urlEncode)

import qualified Crypto.Scrypt as Scrypt
import qualified Data.Text as ST

import Thentos.Types


-- * scratch

import Crypto.BCrypt
import Control.Monad
import Data.Text.Encoding
import Data.Text.ICU.Convert
import Data.Functor.Infix ((<$$>))

-- the puzzle: go to the repo root and run `make pytest`.  you will get a pair of plaintext and
-- bcrypt-protected password in the fashion that works for a3.  now get hs-bcrypt as called in the
-- following lines to verify that pair successfully.
--
-- related: https://github.com/liqd/adhocracy3.mercator/pull/1808 is about a script that exports the
-- data from adhocracy3.  in master: ./bin/export_mercator_users;
-- ./src/adhocracy_mercator/adhocracy_mercator/scripts/export_users.py.  `make pytest` has been
-- checked to generate compatible data with what the script extracts.

good :: (SBS, SBS)
good = ("wefwefwef", "$2a$10$VgGCKVJAyHBTtflKihqGC.fGJ5j7sIZwptAKTfBPsjygS3xjzb1a2")

bad :: (SBS, SBS)
bad = ("incorrect", "$2a$10$KLJDd7d6VCKSIPcDA3QHGu9tYpfnOTSvgueXu/JmwlxfWY4fDOAlW")

-- run a test pair of plain and protected string through hs-bcrypt on all possible encodings.
-- prints a boolean that states whether the pair was good (plaintext matches protected string in
-- some encoding).
mainTest :: (SBS, SBS) -> IO ()
mainTest (m, c) = do
  ms <- allEncodings m
  let valid = or $ (Crypto.BCrypt.validatePassword c) <$> ms
  print valid

-- come up with as many encodings as i could think of.  one of them should be the one used in
-- cryptacular.
allEncodings :: SBS -> IO [SBS]
allEncodings m = do
    let easy = [ m
               , encodeUtf8 $ cs m
               , encodeUtf16LE $ cs m
               , encodeUtf16BE $ cs m
               , encodeUtf32LE $ cs m
               , encodeUtf32BE $ cs m
               ]
    icu <- forM converterNames $ \cnvnm -> (\cnv -> fromUnicode cnv $ cs m) <$> open cnvnm Nothing
    -- print $ length $ nub icu  -- ==> 17
    return $ easy ++ icu

main :: IO ()
main = do
  putStrLn "should be True-False:"
  mainTest good
  mainTest bad

hp :: HashingPolicy
hp = HashingPolicy 10 "$2a$"

-- look at what hs-bcrypt does in all encodings.  (not very informative, as salting should destroy
-- all patterns should there be any.)
q :: IO ()
q = do
    xs :: [SBS] <- allEncodings $ fst good
    ys <- forM xs $ hashPasswordUsingPolicy hp
    mapM_ print $ {- filter (== Just (snd good)) . -} nub . sort $ ys



-- once this has worked out: change the 'verifyPass' below such that it accepts both scrypt and
-- bcrypt-encoded passwords (the latter as delivered by a3).  confirm that checking both in turn is
-- not a significant performance penalty (i think if the password does not match, at least bcrypt is
-- very fast).






-- * crypto

-- | @[2 2 1]@ is fast, but does not provide adequate
-- protection for passwords in production mode!
thentosScryptParams :: Scrypt.ScryptParams
thentosScryptParams = Scrypt.defaultParams
-- thentosScryptParams = fromJust $ Scrypt.scryptParams 2 1 1

hashUserPass :: (Functor m, MonadIO m) => UserPass -> m (HashedSecret UserPass)
hashUserPass = hashSecret fromUserPass

hashServiceKey :: (Functor m, MonadIO m) => ServiceKey -> m (HashedSecret ServiceKey)
hashServiceKey = hashSecret fromServiceKey

-- | 'encryptPassIO'' gets its entropy from /dev/urandom
hashSecret :: (Functor m, MonadIO m) => (a -> ST) -> a -> m (HashedSecret a)
hashSecret a s = HashedSecret <$>
    (liftIO . Scrypt.encryptPassIO thentosScryptParams . Scrypt.Pass . encodeUtf8 $ a s)

secretMatches :: ST -> HashedSecret a -> Bool
secretMatches t s = Scrypt.verifyPass' (Scrypt.Pass $ encodeUtf8 t)
                                       (fromHashedSecret s)

verifyPass :: UserPass -> User -> Bool
verifyPass pass user = secretMatches (fromUserPass pass)
                                     (user ^. userPassword)

verifyKey :: ServiceKey -> Service -> Bool
verifyKey key service = secretMatches (fromServiceKey key)
                                      (service ^. serviceKey)

makeUserFromFormData :: (Functor m, MonadIO m) => UserFormData -> m User
makeUserFromFormData userData = do
    hashedPassword <- hashUserPass $ udPassword userData
    return $ User (udName userData)
                  hashedPassword
                  (udEmail userData)

-- * networking

-- | Encode a bytestring in such a way that it can be used as local part in an email address.
-- This is done by percent-encoding the input in such a way that it could be used in a query string
-- and additionally replacing every "." by "+", since the local address part cannot contain
-- multiple subsequent dots or start or end with a dot.
mailEncode :: ConvertibleStrings s SBS => s -> ST
mailEncode = ST.replace "." "+" . cs . urlEncode True . cs


-- * misc

-- | Convertible show.
cshow :: (Show a, ConvertibleStrings String b) => a -> b
cshow = cs . show


-- | Generic 'readsPrec' for enumerable types.
readsPrecEnumBoundedShow :: (Enum a, Bounded a, Show a) => Int -> String -> [(a, String)]
readsPrecEnumBoundedShow _ s = f [minBound..]
  where
    f [] = []
    f (x:xs) = case splitAt (length s') s of
        (s0, s1) -> if s0 == s' then [(x, s1)] else f xs
      where
        s' = show x


-- | Like 'fmapL' from "Data.EitherR", but with the update of the
-- left value constructed in an impure action.
fmapLM :: (Monad m, Functor m) => (a -> m b) -> Either a r -> m (Either b r)
fmapLM trans (Left e) = Left <$> trans e
fmapLM _ (Right s) = return $ Right s


-- | Like 'fmapLT' from "Data.EitherR", but with the update of the
-- left value constructed in an impure action.
fmapLTM :: (Monad m, Functor m) => (a -> m b) -> EitherT a m r -> EitherT b m r
fmapLTM trans e = EitherT $ do
    result <- runEitherT e
    case result of
        Right r -> return $ Right r
        Left l -> Left <$> trans l

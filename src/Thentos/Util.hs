{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Thentos.Util
    ( makeUserFromFormData
    , verifyPass
    , verifyKey
    , secretMatches
    , hashUserPass
    , hashServiceKey
    , cshow
    , readsPrecEnumBoundedShow
    , (<//>)
) where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.String.Conversions (ConvertibleStrings, ST, cs, (<>))
import Data.Text.Encoding (encodeUtf8)

import Thentos.Types

import qualified Crypto.Scrypt as Scrypt
import qualified Data.Text as ST

-- | @[2 2 1]@ is fast, but does not provide adequate
-- protection for passwords in production mode!
thentosScryptParams :: Scrypt.ScryptParams
thentosScryptParams = Scrypt.defaultParams
-- thentosScryptParams = fromJust $ Scrypt.scryptParams 2 1 1

hashUserPass :: (Functor m, MonadIO m) => UserPass -> m (HashedSecret UserPass)
hashUserPass = hashSecret fromUserPass

hashServiceKey :: (Functor m, MonadIO m) => ServiceKey -> m (HashedSecret ServiceKey)
hashServiceKey = hashSecret fromServiceKey

-- encryptPassIO' gets its entropy from /dev/urandom
hashSecret :: (Functor m, MonadIO m) => (a -> ST) -> a -> m (HashedSecret a)
hashSecret a s = HashedSecret <$>
    (liftIO . Scrypt.encryptPassIO thentosScryptParams . Scrypt.Pass . encodeUtf8 $ a s)

makeUserFromFormData :: (Functor m, MonadIO m) => UserFormData -> m User
makeUserFromFormData userData = do
    hashedPassword <- hashUserPass $ udPassword userData
    return $ User (udName userData)
                  hashedPassword
                  (udEmail userData)
                  []
                  Nothing
                  []

secretMatches :: ST -> HashedSecret a -> Bool
secretMatches t s = Scrypt.verifyPass' (Scrypt.Pass $ encodeUtf8 t)
                                       (fromHashedSecret s)

verifyPass :: UserPass -> User -> Bool
verifyPass pass user = secretMatches (fromUserPass pass)
                                     (user ^. userPassword)

verifyKey :: ServiceKey -> Service -> Bool
verifyKey key service = secretMatches (fromServiceKey key)
                                      (service ^. serviceKey)


-- | Convertible show.
--
-- Remove once https://github.com/soenkehahn/string-conversions/pull/1
-- has been released.
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


-- | Path concatenation for avoiding double slashes in paths.  One
-- optional '/' trailing left side / leading right side is removed,
-- and one '/' is inserted.
(<//>) :: (ConvertibleStrings s ST, ConvertibleStrings ST s) => s -> s -> s
(cs -> p) <//> (cs -> p') = cs $ q <> "/" <> q'
  where
    q  :: ST = if "/" `ST.isSuffixOf` p  then ST.init p  else p
    q' :: ST = if "/" `ST.isPrefixOf` p' then ST.tail p' else p'

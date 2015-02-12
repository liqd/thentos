{-# LANGUAGE FlexibleContexts #-}

module Util
    ( makeUserFromFormData
    , verifyPass
    , secretMatches
    , hashServiceKey
    , hashUserPass
    , cshow
    , readsPrecEnumBoundedShow
) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Scrypt (encryptPassIO', Pass(Pass), verifyPass')
import Data.String.Conversions (ConvertibleStrings, ST, cs)
import Data.Text.Encoding (encodeUtf8)
import Types

hashUserPass :: MonadIO m => UserPass -> m (HashedSecret UserPass)
hashUserPass = hashSecret fromUserPass

hashServiceKey :: MonadIO m => ServiceKey -> m (HashedSecret ServiceKey)
hashServiceKey = hashSecret fromServiceKey

-- encryptPassIO' gets its entropy from /dev/urandom
hashSecret :: MonadIO m => (a -> ST) -> a -> m (HashedSecret a)
hashSecret a s =
    (liftIO . encryptPassIO' . Pass . encodeUtf8 $ a s) >>= return . HashedSecret

makeUserFromFormData :: MonadIO m => UserFormData -> m User
makeUserFromFormData userData = do
    hashedPassword <- hashUserPass $ udPassword userData
    return $ User (udName userData)
                  hashedPassword
                  (udEmail userData)
                  []
                  Nothing
                  []

secretMatches :: ST -> HashedSecret a -> Bool
secretMatches t s = verifyPass' (Pass $ encodeUtf8 t) (fromHashedSecret s)

verifyPass :: UserPass -> User -> Bool
verifyPass pass user = secretMatches (fromUserPass pass)
                                     (user ^. userPassword)

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

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}

module Thentos.Util
    ( hashUserPass
    , makeUserFromFormData
    , verifyUserPass
    , hashServiceKey
    , verifyServiceKey
    , hashSecret
    , hashSecretWith
    , mailEncode
    , cshow
    , readsPrecEnumBoundedShow
) where

import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (urlEncode)

import qualified Crypto.BCrypt as BCrypt
import qualified Crypto.Scrypt as Scrypt
import qualified Data.Text as ST

import Thentos.Prelude
import Thentos.Types


-- * crypto

hashUserPass :: (Functor m, MonadIO m) => UserPass -> m (HashedSecret UserPass)
hashUserPass = hashSecret fromUserPass

makeUserFromFormData :: (Functor m, MonadIO m) => UserFormData -> m User
makeUserFromFormData userData = do
    hashedPassword <- hashUserPass $ udPassword userData
    return $ User (udName userData)
                  hashedPassword
                  (udEmail userData)

verifyUserPass :: UserPass -> User -> Bool
verifyUserPass pass user = secretMatches (fromUserPass pass) (user ^. userPassword)

hashServiceKey :: (Functor m, MonadIO m) => ServiceKey -> m (HashedSecret ServiceKey)
hashServiceKey = hashSecret fromServiceKey

verifyServiceKey :: ServiceKey -> Service -> Bool
verifyServiceKey key service = secretMatches (fromServiceKey key) (service ^. serviceKey)


-- | Call 'hasSecretWith' with fresh salt and default params.
hashSecret :: (Functor m, MonadIO m) => (a -> ST) -> a -> m (HashedSecret a)
hashSecret a s = (\salt -> hashSecretWith Scrypt.defaultParams salt a s) <$> liftIO Scrypt.newSalt

hashSecretWith :: Scrypt.ScryptParams -> Scrypt.Salt -> (a -> ST) -> a -> HashedSecret a
hashSecretWith params salt a =
    SCryptHash . Scrypt.getEncryptedPass .
    Scrypt.encryptPass params salt . Scrypt.Pass . encodeUtf8 . a

secretMatches :: ST -> HashedSecret a -> Bool
secretMatches t s = case s of
    SCryptHash hash -> Scrypt.verifyPass' (Scrypt.Pass $ encodeUtf8 t) (Scrypt.EncryptedPass hash)
    BCryptHash hash -> BCrypt.validatePassword hash (encodeUtf8 t)


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

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Thentos.CookieSession.CSRF
    ( CsrfSecret(..)
    , CsrfToken(..)
    , CsrfNonce(..)
    , GetCsrfSecret(..)
    , HasSessionCsrfToken(..)
    , MonadHasSessionCsrfToken
    , MonadViewCsrfSecret
    , genCsrfSecret
    , validFormatCsrfSecretField
    , validFormatCsrfToken
    , checkCsrfToken
    , refreshCsrfToken
    , clearCsrfToken
    ) where

import Control.Lens
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad (when)
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC,hmac)
import Crypto.Random (MonadRandom(..))
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteArray.Encoding (convertToBase, convertFromBase, Base(Base16))
import Data.String.Conversions (SBS, ST, cs, (<>))
import Data.String (IsString)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import qualified Data.ByteString as SBS
import qualified Data.Text as ST

import Servant.Missing (MonadError500, throwError500)
import Thentos.CookieSession.Types (ThentosSessionToken(fromThentosSessionToken), MonadUseThentosSessionToken, getThentosSessionToken)

-- | This token is used to prevent CSRF (Cross Site Request Forgery).
-- This token is part of 'FrontendSessionData' since it is required by the views which
-- generate the forms with a special hidden field containing the value of this token.
-- However, this token is cleared before being serialized as a cookie.
-- Indeed we have no need yet to have it on the client side nor to make it persistent.
-- When processing requests, this token is freshly generated from the 'CsrfSecret' and the
-- 'ThentosSessionToken'. This token is only used by requests that yield an HTML form.
-- Upon POST requests on such forms, the handlers will check the validity of the CSRF token.
-- Verification of this token can be done solely from the 'CsrfSecret' and
-- the 'ThentosSessionToken'.
--
-- This all means that if the attacker could get access to one of these tokens it would be enough to
-- validate any form.  Changing the token on every request even inside the session helps to counter
-- an attack based on entropy leakage through TLS plaintext compression.  TLS compression should be
-- disabled for the exact reason that it is vulnerable to this attack, but this code does not rely
-- on it.
--
-- *The idea of the attack:* When combining encryption (which hides the contents but not the length)
-- and compression (which makes the length depend on the content).  If you compress after encryption
-- its safe but useless; if you compress then encrypt (which is often done), then some data leaks
-- through the length of the ciphertext.  The BEAST attack exploited this by guessing the CSRF token
-- by sending request to the server pretending to be the client injecting in the request the guess
-- of the token in a parameter which is echoed back by the server to the real client encrypted for
-- the client.  Assuming the CSRF token is given as an attribute such as csrf:SOMESECRET to keep it
-- simple, then the guess is going to be csrf:XYZ with all combination of XYZ then you look at which
-- answer was the shortest it is highly likely that XYZ=SOM will compress better than the rest
-- because of the repetition with the real secret also part of the response. You then proceed with
-- your guess being csrf:SOMXYZ. On a test setup, it was possible to recover the full token in 30
-- secs.
newtype CsrfToken = CsrfToken { fromCsrfToken :: ST }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON, Typeable, Generic, IsString)

newtype CsrfSecret = CsrfSecret SBS
    deriving (Show, Eq)

newtype CsrfNonce  = CsrfNonce  SBS
    deriving (Show, Eq)

class GetCsrfSecret a where
    csrfSecret :: Getter a (Maybe CsrfSecret)

instance GetCsrfSecret ST.Text where
    csrfSecret = to $ \secret -> do
        let Right sbs = convertFromBase Base16 (cs secret :: SBS)
        return $ CsrfSecret sbs

class HasSessionCsrfToken a where
    sessionCsrfToken :: Lens' a (Maybe CsrfToken)

type MonadHasSessionCsrfToken s m = (MonadState s m, HasSessionCsrfToken s)

type MonadViewCsrfSecret e m = (MonadReader e m, GetCsrfSecret e)

-- | This ONLY checks the format of a given CSRF secret, not if it has been randomly choosen (duh!).
validFormatCsrfSecretField :: Maybe ST -> Bool
validFormatCsrfSecretField ms
    | Just t <- ms
    , Right s' <- convertFromBase Base16 (cs t :: SBS)
    = SBS.length s' == 32
    | otherwise = False

-- | This ONLY checks the format of a given CSRF token, not if it has been tampered with.
validFormatCsrfToken :: CsrfToken -> Bool
validFormatCsrfToken (CsrfToken st)
    | Right s' <- convertFromBase Base16 (cs st :: SBS) = SBS.length s' == 64
    | otherwise                                         = False

-- | Computes a valid CSRF token given a nonce.
makeCsrfToken :: (MonadError500 err m, MonadViewCsrfSecret e m, MonadUseThentosSessionToken s m) =>
                 CsrfNonce -> m CsrfToken
makeCsrfToken (CsrfNonce rnd) = do
    maySessionToken <- use getThentosSessionToken
    case maySessionToken of
        Nothing -> throwError500 "No session token"
        Just sessionToken -> do
            Just (CsrfSecret key) <- view csrfSecret
            return $ CsrfToken . cs $ rnd <> convertToBase Base16 (hmac key (tok <> rnd) :: HMAC SHA256)
          where
            tok = cs $ fromThentosSessionToken sessionToken

-- | Extracts the nonce part from the CSRF token.
csrfNonceFromCsrfToken :: CsrfToken -> CsrfNonce
csrfNonceFromCsrfToken = CsrfNonce . SBS.take 64 . cs . fromCsrfToken

-- | Verify the authenticity of a given 'CsrfToken'.  This token should come from the form data of
-- the POST request, NOT from 'FrontendSessionData'.
checkCsrfToken :: (MonadError500 err m, MonadViewCsrfSecret e m, MonadUseThentosSessionToken s m) => CsrfToken -> m ()
checkCsrfToken csrfToken
    | not (validFormatCsrfToken csrfToken) =
        throwError500 $ "Ill-formatted CSRF Token " <> show csrfToken
    | otherwise = do
        -- Here we essentially re-create the second half of the CSRF token.
        -- If it was made with the same sessionToken and csrfSecret then it will match.
        csrfToken' <- makeCsrfToken (csrfNonceFromCsrfToken csrfToken)
        when (csrfToken /= csrfToken') $
            throwError500 "Invalid CSRF token"

-- | Generates a random 'CsrfSecret'.
genCsrfSecret :: MonadRandom m => m CsrfSecret
genCsrfSecret = CsrfSecret . (convertToBase Base16 :: SBS -> SBS) <$> getRandomBytes 32

-- | Generates a random 'CsrfNonce'.
genCsrfNonce :: MonadRandom m => m CsrfNonce
genCsrfNonce = CsrfNonce . (convertToBase Base16 :: SBS -> SBS) <$> getRandomBytes 32

-- | See 'CsrfToken'.
-- This function assigns a newly generated 'CsrfToken' to the 'FrontendSessionData'.
refreshCsrfToken :: (MonadError500 err m, MonadHasSessionCsrfToken s m,
                     MonadRandom m, MonadViewCsrfSecret e m, MonadUseThentosSessionToken s m) => m ()
refreshCsrfToken = do
    csrfToken <- makeCsrfToken =<< genCsrfNonce
    sessionCsrfToken .= Just csrfToken

-- | See 'CsrfToken'
-- As long as we do not need to generate any forms on the client side it's better to
-- clear the value of the 'CsrfToken', preventing it to be stored in the cookie.
-- Still if we ever need to persist a 'CsrfToken' it might be safer to refresh it
-- with 'refreshCsrfToken' first.
clearCsrfToken :: MonadHasSessionCsrfToken s m => m ()
clearCsrfToken = sessionCsrfToken .= Nothing

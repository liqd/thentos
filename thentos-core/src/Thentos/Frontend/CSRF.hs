{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Thentos.Frontend.CSRF
    ( CsrfSecret(CsrfSecret)
    , genCsrfSecret
    , validFormatCsrfSecretField
    , checkCsrfToken
    , refreshCsrfToken
    , clearCsrfToken
    ) where

import "cryptonite" Crypto.Hash (SHA256)
import "cryptonite" Crypto.MAC.HMAC (HMAC,hmac)
import "cryptonite" Crypto.Random (MonadRandom(getRandomBytes))
import Data.Configifier ((>>.))
import Data.ByteArray.Encoding (convertToBase, convertFromBase, Base(Base16))

import qualified Data.ByteString as SBS

import Thentos.Prelude hiding (MonadRandom)
import Thentos.Frontend.Types
import Thentos.Types (ThentosSessionToken(fromThentosSessionToken))

import qualified Thentos.Action.Unsafe as U

newtype CsrfSecret = CsrfSecret SBS
    deriving (Show)
newtype CsrfNonce  = CsrfNonce  SBS

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
makeCsrfToken :: CsrfNonce -> FAction CsrfToken
makeCsrfToken (CsrfNonce rnd) = do
    maySessionToken <- preuse $ fsdLogin . _Just . fslToken
    case maySessionToken of
        Nothing -> crash $ FActionError500 "No session token"
        Just sessionToken -> do
            CsrfSecret key <- getCsrfSecret
            return $ CsrfToken . cs $ rnd <> convertToBase Base16 (hmac key (tok <> rnd) :: HMAC SHA256)
          where
            tok = cs $ fromThentosSessionToken sessionToken

-- | Extracts the nonce part from the CSRF token.
csrfNonceFromCsrfToken :: CsrfToken -> CsrfNonce
csrfNonceFromCsrfToken = CsrfNonce . SBS.take 64 . cs . fromCsrfToken

-- | Verify the authenticity of a given 'CsrfToken'.  This token should come from the form data of
-- the POST request, NOT from 'FrontendSessionData'.
checkCsrfToken :: CsrfToken -> FAction ()
checkCsrfToken csrfToken
    | not (validFormatCsrfToken csrfToken) =
        crash . FActionError500 $ "Ill-formatted CSRF Token " <> show csrfToken
    | otherwise = do
        -- Here we essentially re-create the second half of the CSRF token.
        -- If it was made with the same sessionToken and csrfSecret then it will match.
        csrfToken' <- makeCsrfToken (csrfNonceFromCsrfToken csrfToken)
        when (csrfToken /= csrfToken') $
            crash $ FActionError500 "Invalid CSRF token"

-- | Generates a random 'CsrfSecret'.
genCsrfSecret :: MonadRandom m => m CsrfSecret
genCsrfSecret = CsrfSecret . (convertToBase Base16 :: SBS -> SBS) <$> getRandomBytes 32

-- | Generates a random 'CsrfNonce'.
genCsrfNonce :: FAction CsrfNonce
genCsrfNonce = CsrfNonce . convertToBase Base16 <$> U.unsafeAction (U.genRandomBytes 32)

-- | See 'CsrfToken'.
-- This function assigns a newly generated 'CsrfToken' to the 'FrontendSessionData'.
refreshCsrfToken :: FAction ()
refreshCsrfToken = do
    csrfToken    <- makeCsrfToken =<< genCsrfNonce
    fsdCsrfToken .= Just csrfToken

-- | See 'CsrfToken'
-- As long as we do not need to generate any forms on the client side it's better to
-- clear the value of the 'CsrfToken', preventing it to be stored in the cookie.
-- Still if we ever need to persist a 'CsrfToken' it might be safer to refresh it
-- with 'refreshCsrfToken' first.
clearCsrfToken :: FAction ()
clearCsrfToken = fsdCsrfToken .= Nothing

-- | Get the 'CsrfSecret' from the configuration.
getCsrfSecret :: FAction CsrfSecret
getCsrfSecret = do
    Just (secret :: ST) <- (>>. (Proxy :: Proxy '["csrf_secret"])) <$> U.unsafeAction U.getConfig
    let Right sbs = convertFromBase Base16 (cs secret :: SBS)
    return $ CsrfSecret sbs

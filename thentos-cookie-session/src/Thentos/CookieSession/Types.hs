{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}

module Thentos.CookieSession.Types where

import Control.Lens (Getter)
import Control.Monad.State.Class (MonadState)
import "cryptonite" Crypto.Random (MonadRandom, getRandomBytes)
import Data.Aeson (FromJSON, ToJSON)
import Data.String.Conversions
import Data.String (IsString)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Servant.API (FromHttpApiData)
import qualified Codec.Binary.Base64 as Base64
import qualified Data.Text as ST

newtype ThentosSessionToken = ThentosSessionToken { fromThentosSessionToken :: ST }
    deriving ( Eq, Ord, Show, Read, Typeable, Generic, IsString
             , FromHttpApiData, FromJSON, ToJSON
             )

class GetThentosSessionToken a where
    getThentosSessionToken :: Getter a (Maybe ThentosSessionToken)

type MonadUseThentosSessionToken s m = (MonadState s m, GetThentosSessionToken s)

-- | Return a base64 encoded random string of length 24 (18 bytes of entropy).
-- We use @_@ instead of @/@ as last letter of the base64 alphabet since it allows using names
-- within URLs without percent-encoding. Our Base64 alphabet thus consists of ASCII letters +
-- digits as well as @+@ and @_@. All of these are reliably recognized in URLs, even if they occur
-- at the end.
--
-- RFC 4648 also has a "URL Safe Alphabet" which additionally replaces @+@ by @-@. But that's
-- problematic, since @-@ at the end of URLs is not recognized as part of the URL by some programs
-- such as Thunderbird.
freshRandomName :: MonadRandom m => m ST
freshRandomName = ST.replace "/" "_" . cs . Base64.encode <$> getRandomBytes 18

freshSessionToken :: MonadRandom m => m ThentosSessionToken
freshSessionToken = ThentosSessionToken <$> freshRandomName

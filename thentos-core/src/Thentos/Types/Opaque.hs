-- | Types whose internal representation is not accessible.
module Thentos.Types.Opaque
    ( Random64, mkRandom64, fromRandom64
    )
where

import Data.String.Conversions (SBS)

import qualified Data.ByteString as SB


-- | 64 bytes of randomness.
newtype Random64 = Random64 SBS
    deriving (Eq, Ord, Show)

-- | Construct a 'Random64' from a bytestring. Returns 'Just' a Random64 wrapping the input
-- if its length is 64, 'Nothing' otherwise.
mkRandom64 :: SBS -> Maybe Random64
mkRandom64 bs = if SB.length bs == 64 then Just $ Random64 bs else Nothing

-- | Extract the wrapped 64 bytes from a 'Random64'.
fromRandom64 :: Random64 -> SBS
fromRandom64 (Random64 bs) = bs

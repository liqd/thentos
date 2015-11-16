-- | Types whose internal representation is not accessible.
module Thentos.Types.Opaque
    ( Random20, mkRandom20, fromRandom20
    )
where

import Data.String.Conversions (SBS)

import qualified Data.ByteString as SB


-- | 20 bytes of randomness.
-- For comparison: an UUID has 16 bytes, so that should be enough for all practical purposes.
newtype Random20 = Random20 SBS
    deriving (Eq, Ord, Show)

-- | Construct a 'Random20' from a bytestring. Returns 'Just' a Random20 wrapping the input
-- if its length is 20, 'Nothing' otherwise.
mkRandom20 :: SBS -> Maybe Random20
mkRandom20 bs = if SB.length bs == 20 then Just $ Random20 bs else Nothing

-- | Extract the wrapped 20 bytes from a 'Random20'.
fromRandom20 :: Random20 -> SBS
fromRandom20 (Random20 bs) = bs

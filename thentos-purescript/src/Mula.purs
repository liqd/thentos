module Mula where

import Halogen (HTML())
import Halogen.HTML.Indexed (text)

-- FIXME: result should be MuLa newtype, not bare html.
translate :: forall p i. String -> HTML p i
translate msg = text msg

-- FIXME: ...  in fact, for attributes, we need this to be not HTML at all!
translateS :: String -> String
translateS msg = msg

module Mula where

import Halogen (HTML())
import Halogen.HTML.Indexed (text)

translate :: forall p i. String -> HTML p i
translate msg = text msg

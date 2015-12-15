module Mula (tr, trh) where

import Prelude ((<<<))
import Halogen.HTML.Indexed (text)

-- | Turn a translation key into a translated string.
foreign import tr :: String -> String

-- | Convenience for @'text' <<< 'translateS'@.
trh :: forall p i. String -> Halogen.HTML.Indexed.HTML p i
trh = text <<< tr

-- FIXME:
-- The effect type for changing or querying the translation language.
-- foreign import data I18N :: !
-- foreign import setLanguage :: forall eff. String -> Aff (i18n : I18N | eff) Unit
-- foreign import getLanguage :: forall eff. Aff (i18n :: I18N | eff) String

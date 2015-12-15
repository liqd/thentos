module I18n (tr, trh) where

import Prelude ((<<<), show)
import Halogen.HTML.Indexed (text)

import qualified I18n.Lang as Lang


-- | Turn a translation key into a translated string.
tr :: String -> String
tr = trF (show Lang.EN)

-- | Convenience for @'text' <<< 'tr'@.
trh :: forall p i. String -> Halogen.HTML.Indexed.HTML p i
trh = text <<< tr

foreign import trF :: String -> String -> String

-- FIXME:
-- The effect type for changing or querying the translation language.
-- foreign import data I18N :: !
-- foreign import setLanguage :: forall eff. String -> Aff (i18n : I18N | eff) Unit
-- foreign import getLanguage :: forall eff. Aff (i18n :: I18N | eff) String

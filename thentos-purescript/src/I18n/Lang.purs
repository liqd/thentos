module I18n.Lang
where

import Data.Generic
import Data.Maybe
import Data.String
import Prelude

-- FUTURE WORK: use ISO639, but make sure that lib user can select a subset of languages that will
-- allow for static translation coverage checks.
--
-- see also: https://github.com/zerobuzz/multi-language/blob/master/src/MuLa/ISO639.hs

data Lang = EN | DE

derive instance genericLang :: Generic Lang
instance eqLang :: Eq Lang where eq = gEq
instance showLang :: Show Lang where show = dropQualification <<< gShow

dropQualification :: String -> String
dropQualification s = case lastIndexOf "." s of
    Nothing -> s
    Just i -> drop (i+1) s

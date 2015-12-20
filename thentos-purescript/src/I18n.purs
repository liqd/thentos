-- | Pure translation functions.  The function `trH` can be called once after the entire HTML tree
-- has been constructed.  The language key always needs to be passed as explicit function argument.
-- Properties may not contain translation keys; only elements are translated by `trH`.  If you need
-- to pass translated texts to properties, call `trS` directly on the translation key.
--
-- For making sure the translation tables in `I18n.js` are all up to date (i.e., are not missing any
-- translations and contain no unused ones), check out `$THENTOS_DEV_ROOT/refresh-i18n/`.
--
-- NOTE: We could set the language by passing a function like `function () { return 'EN' }` into the
-- component via the config.  This function would be called every time a translation key is looked
-- up.  This makes sense if you look at it with an angular-js mindset, but there are two
-- disadvantages to this approach:
-- 1. performance, especially if the function has non-trivial run time;
-- 2. it is brittle and error-prone.  Language change is an event, and the component needs a chance
--    to react to this event.  If the function does a look-up into a global variable maintained in
--    another web framework and that global variable changes, we have no way of knowing that we need
--    to re-render.
--
-- FUTUREWORK:
--
-- - translate strings in element Properties (see 0ec877a4 for a first try).  something like neil
--   mitchell's uniplate may come in handy there.
--
-- - consider transformer on CompontentHTML that carries the language / state.  make it a fixme or
--   implement it, or explain why we don't do it.
--
-- - offer a language key "debug" that translates everything to "*****".  this can be used to
--   navigate the app and hunt down untranslated text fragments.  hm.  something more automatic
--   would be nice, but it's hard.
module I18n (trH, trS) where

import Data.String (take, length)
import Halogen.Component (ComponentHTML())
import Halogen.HTML.Indexed (HTML(Slot, Element, Text))
import Prelude (show, otherwise, (==), (<$>), ($))

import Error
import qualified I18n.Lang as Lang

trH :: forall f. Lang.Lang -> ComponentHTML f -> ComponentHTML f
trH lang (Text txt)                  = Text $ trSLenient lang txt
trH lang (Element ns tagn attrs chs) = Element ns tagn attrs $ trH lang <$> chs
trH lang (Slot p)                    = throwJS "I18n.trH: cannot translate Slots!"  -- FIXME: can I?

trS :: Lang.Lang -> String -> String
trS lang txt = trF (show lang) txt

trSLenient :: Lang.Lang -> String -> String
trSLenient lang txt
    | "TR__" `isPrefixOf` txt = trF (show lang) txt
    | otherwise               = txt

foreign import trF :: String -> String -> String

-- | FIXME: write PR for package purescript-strings; also include isSuffixOf, isInfixOf.
isPrefixOf :: String -> String -> Boolean
isPrefixOf prefix s = take (length prefix) s == prefix

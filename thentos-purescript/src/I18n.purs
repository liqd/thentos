-- | Pure translation functions.  The function `trH` can be called once after the entire HTML tree
-- has been constructed.  The language key always needs to be passed as explicit function argument.
--
-- FUTURE WORK: something like neil mitchell's uniplate would come in handy here.
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


-- TODO: implement and export language change event.  add buttons to test this to js/register.html.

-- TODO: think about a way to offer a3 a socket for a function that always returns the current
-- language, and then call that function every N ms and trigger the event if language actually
-- changed.  boy, this is really strange.  NEW PLAN: don't do any of this, but discuss the reason
-- why this is a bad idea (language change is an event).

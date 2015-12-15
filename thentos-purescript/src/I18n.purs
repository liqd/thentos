-- | Pure translation functions.  The function `trH` can be called once after the entire HTML tree
-- has been constructed.  The language key always needs to be passed as explicit function argument.
--
-- FUTURE WORK: something like neil mitchell's uniplate would come in handy here.
module I18n (trH, trS) where

import Data.ExistsR (runExistsR, mkExistsR)
import Data.Exists (runExists, mkExists)
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (take, length)
import Data.Tuple (Tuple(Tuple))
import Halogen.Component (ComponentHTML())
import Halogen.HTML.Indexed ( HTML(Slot, Element, Text), Prop(..), PropF(..), HandlerF(..)
                            , runEventName, eventName
                            )
import Prelude (Unit(), show, otherwise, (==), (<$>), (<>), ($), (<<<))

import Error
import qualified I18n.Lang as Lang

trH :: forall f. Lang.Lang -> ComponentHTML f -> ComponentHTML f
trH lang (Text txt)                  = Text $ trSLenient lang txt
trH lang (Element ns tagn attrs chs) = Element ns tagn (trProp lang <$> attrs) (trH lang <$> chs)
trH lang (Slot p)                    = throwJS "I18n.trH: cannot translate Slots!"  -- FIXME: can I?

trS :: Lang.Lang -> String -> String
trS lang txt
    | "TR__" `isPrefixOf` txt = trF (show lang) txt
    | otherwise               = throwJS $ "I18n.trString: bad translation key: " <> txt

trSLenient :: Lang.Lang -> String -> String
trSLenient lang txt
    | "TR__" `isPrefixOf` txt = trF (show lang) txt
    | otherwise               = txt

-- FIXME: trProp, trPropF, trHandlerF are missing e.g. submit button text.  can probably be fixed?

trProp :: forall f. Lang.Lang -> Prop (f Unit) -> Prop (f Unit)
trProp _    p@(Finalizer _)   = p
trProp _    p@(Initializer _) = p
trProp _    p@(Handler exHF)  = Handler (runExistsR (mkExistsR <<< trHandlerF lang) exHF)
trProp lang (Key key)         = throwJS ("I18n.trProp: cannot translate Key " <> key)
trProp lang (Attr ns key val) = Attr ns key (trSLenient lang val)
trProp lang (Prop exPropF)    = Prop (runExists (mkExists <<< trPropF lang) exPropF)

trPropF :: forall value. Lang.Lang -> PropF value -> PropF value
trPropF _    (PropF pName value Nothing) = throwJS "I18n.trPropF: render is Nothing"
trPropF lang (PropF pn v (Just (Tuple an render))) = PropF pn v (Just (Tuple an render'))
  where
    render' a' pn' v' = trSLenient lang $ render a' pn' v'

trHandlerF :: forall i fields. Lang.Lang -> HandlerF i fields -> HandlerF i fields
trHandlerF lang (HandlerF n f) = HandlerF (eventName (trSLenient lang (runEventName n))) f

foreign import trF :: String -> String -> String

-- | FIXME: write PR for package purescript-strings; also include isSuffixOf, isInfixOf.
isPrefixOf :: String -> String -> Boolean
isPrefixOf prefix s = take (length prefix) s == prefix


-- TODO: implement and export language change event.  add buttons to test this to js/register.html.

-- TODO: think about a way to offer a3 a socket for a function that always returns the current
-- language, and then call that function every N ms and trigger the event if language actually
-- changed.  boy, this is really strange.  NEW PLAN: don't do any of this, but discuss the reason
-- why this is a bad idea (language change is an event).

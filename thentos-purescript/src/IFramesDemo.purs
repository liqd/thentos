module IFramesDemo where

import Prelude

import Control.Monad.Aff (Aff(), Canceler(), runAff, forkAff)
import Control.Monad.Aff.Class (MonadAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Data.Array (replicate)
import Halogen (Component(), ComponentHTML(), ComponentDSL(), HalogenEffects(), Natural(),
                component, modify, runUI)
import Halogen.Util (appendToBody)
import Prim (Boolean(), Int(), Number(), String(), Array())

import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P

import Data.Maybe
import Unsafe.Coerce (unsafeCoerce)
import Halogen.HTML.Core (IsProp, prop, propName, attrName)


data IFDState = IFDState Int Int Int

initialIFDState :: IFDState
initialIFDState = IFDState 30 20 1

data IFDQuery a = UpdX Int a | UpdY Int a | UpdZ Int a

foreign import onChangeValue :: forall a. a -> Int

slideHandler :: forall eff. (Int -> Unit -> IFDQuery Unit) -> P.IProp (onInput :: P.I | eff) (IFDQuery Unit)
slideHandler mkQuery = E.onInput $ E.input $ \domEv -> mkQuery (onChangeValue domEv)

ui :: forall g. (Functor g) => Component IFDState IFDQuery g
ui = component render eval
  where
    render :: IFDState -> ComponentHTML IFDQuery
    render state@(IFDState x y z) = H.div_
            [ H.p_ [ H.input [ slideHandler UpdX
                             , P.inputType P.InputRange
                             , P.value (show x), min_ 1, max_ 90
                             ]
                   , H.text $ "x=" ++ show x
                   ]
            , H.p_ [ H.input [ slideHandler UpdY
                             , P.inputType P.InputRange
                             , P.value (show y), min_ 1, max_ 90
                             ]
                   , H.text $ "y=" ++ show y
                   ]
            , H.p_ [ H.input [ slideHandler UpdZ
                             , P.inputType P.InputRange
                             , P.value (show z), min_ 1, max_ 30
                             ]
                   , H.text $ "z=" ++ show z
                   ]
            , H.div_ [renderXY]
            ]
      where
        min_ = unsafeCoerce <<< prop (propName "min") (Just $ attrName "min")
        max_ = unsafeCoerce <<< prop (propName "max") (Just $ attrName "max")

        wh_ :: forall p. Int -> Array (P.IProp (width :: P.I, height :: P.I | p) (IFDQuery Unit))
        wh_ i = [P.width (P.Pixels i), P.height (P.Pixels i)]  -- FIXME: this becomes `width="[object Object]"`

        renderXY :: ComponentHTML IFDQuery
        renderXY = H.table_ <<< replicate (y / 10) <<< H.tr_ <<< replicate (x / 10) $ H.td_ [renderZ]

        renderZ :: ComponentHTML IFDQuery
        renderZ = case (z / 10) of
            0 -> H.iframe [P.src "/js/index.html"]
            1 -> H.iframe $ [P.src "/js/frames1.html"] ++ wh_ 50
            _ -> H.iframe $ [P.src "/js/frames2.html"] ++ wh_ 100

    eval :: Natural IFDQuery (ComponentDSL IFDState IFDQuery g)
    eval (UpdX x next) = do
        modify (\(IFDState _ y z) -> IFDState x y z)
        pure next
    eval (UpdY y next) = do
        modify (\(IFDState x _ z) -> IFDState x y z)
        pure next
    eval (UpdZ z next) = do
        modify (\(IFDState x y _) -> IFDState x y z)
        pure next

type IFDEffects eff = HalogenEffects eff

runner ::  forall eff. Aff (IFDEffects eff) (Canceler (IFDEffects eff))
runner = do
    { node: node, driver: driver } <- runUI ui initialIFDState
    appendToBody node
    forkAff $ pure unit

main :: forall eff. Eff (IFDEffects eff) Unit
main = runAff throwException (const (pure unit)) runner

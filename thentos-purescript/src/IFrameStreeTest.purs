module IFrameStressTest where

import Control.Monad.Aff (Aff(), Canceler(), runAff, forkAff, later', liftEff')
import Control.Monad.Aff.Console (log, print)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Eff.Random (RANDOM(), randomInt)
import Data.Either (Either(Right, Left))
import Halogen (Component(), ComponentHTML(), ComponentDSL(), HalogenEffects(), Natural(),
                component, modify, runUI, action)
import Halogen.Util (appendTo)
import Prelude (Show, Functor, (++), Unit(), show, pure, const, bind, unit, (+), ($), (>>=), (<<<))
import Prim (Boolean(), Int(), String())

import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified DOM.HTML.Types as D


-- counter

newtype CounterState = CounterState Int

initialCounterState :: CounterState
initialCounterState = CounterState 0

data CounterQuery a = Tick a

counterUI :: forall g. (Functor g) => Component CounterState CounterQuery g
counterUI = component render eval
  where
    render :: CounterState -> ComponentHTML CounterQuery
    render (CounterState n) = H.div_ [H.text ("[counter=" ++ show n ++ "]")]

    eval :: Natural CounterQuery (ComponentDSL CounterState CounterQuery g)
    eval (Tick next) = do
        modify (\(CounterState n) -> CounterState (n + 1))
        pure next

verbose :: Boolean
verbose = true

type CounterEffects eff = HalogenEffects (console :: CONSOLE, random :: RANDOM | eff)

counterRunner ::  forall eff a.
    String ->
    Aff (CounterEffects eff) a ->
    Aff (CounterEffects eff) (Canceler (CounterEffects eff))
counterRunner selector callback = forkAff $ do
    { node: node, driver: driver } <- runUI counterUI initialCounterState
    liftEff $ appendTo selector node
    i <- liftEff $ randomInt 500 2000
    setInterval i $ driver (action Tick)
  where
    _log :: forall eff. String -> Aff (console :: CONSOLE | eff) Unit
    _log s = if verbose then log s >>= \_ -> pure unit else pure unit

    setInterval :: forall a.
          Int ->
          Aff (CounterEffects eff) a ->
          Aff (CounterEffects eff) Unit
    setInterval ms action = later' ms $ do
        _log "[counter: tick!]"
        callback
        action
        setInterval ms action

-- TODO: variant of counterRunner with sync callback (in Eff) that doesn't need to return.

counterMain :: forall eff a.
    String ->
    Aff (CounterEffects eff) a ->
    Eff (CounterEffects eff) Unit
counterMain selector callback = runAff throwException (const (pure unit))
    (counterRunner selector callback)

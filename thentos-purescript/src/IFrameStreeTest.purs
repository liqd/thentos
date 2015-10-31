module IFrameStressTest where

import Control.Monad.Aff (Aff(), runAff, later')
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Eff.Random (RANDOM(), randomInt)
import Halogen
import Halogen.Util (appendTo)
import Prelude

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

counterRunner :: forall eff. String -> Aff (HalogenEffects (random :: RANDOM | eff)) Unit
counterRunner widgetRoot = do
    { node: node, driver: driver } <- runUI counterUI initialCounterState
    appendTo widgetRoot node
    i <- liftEff $ randomInt 500 2000
    setInterval i $ driver (action Tick)
  where
    setInterval :: forall e a. Int -> Aff e a -> Aff e Unit
    setInterval ms a = later' ms $ do
      a
      setInterval ms a

counterMain :: forall eff. String -> Eff (HalogenEffects (random :: RANDOM | eff)) Unit
counterMain = runAff throwException (const (pure unit)) <<< counterRunner

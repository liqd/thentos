module Counter where

import Control.Monad.Aff (Aff(), Canceler(), runAff, forkAff, later')
import Control.Monad.Aff.Class (MonadAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Eff.Random (RANDOM(), randomInt)
import Data.Tuple (Tuple(Tuple))
import Halogen (Component(), ComponentHTML(), ComponentDSL(), HalogenEffects(), Natural(), Driver(),
                component, modify, get, runUI, action)
import Halogen.Query (liftAff')
import Halogen.Util (appendTo)
import Prelude (Show, Functor, Unit(), show, pure, const, void, bind, unit, (+), ($), (>>=), (++))
import Prim (Boolean(), Int(), String())

import qualified Halogen.HTML.Indexed as H


-- counter

data CounterState eff = CounterState Int (Aff (CounterEffects eff) Unit)

initialCounterState :: forall eff. Aff (CounterEffects eff) Unit -> CounterState eff
initialCounterState = CounterState 0

data CounterQuery a = Tick a | Clear a

counterUI :: forall eff g. (Functor g, MonadAff (CounterEffects eff) g)
          => Component (CounterState eff) CounterQuery g
counterUI = component render eval
  where
    render :: CounterState eff -> ComponentHTML CounterQuery
    render (CounterState n _) = H.div_ [H.text ("[counter=" ++ show n ++ "]")]

    eval :: Natural CounterQuery (ComponentDSL (CounterState eff) CounterQuery g)
    eval (Tick next) = do
        modify (\(CounterState n h) -> CounterState (n + 1) h)
        CounterState _ tickHandler <- get
        liftAff' $ forkAff tickHandler
        pure next
    eval (Clear next) = do
        modify (\(CounterState _ h) -> CounterState 0 h)
        pure next

verbose :: Boolean
verbose = true

type CounterEffects eff = HalogenEffects (console :: CONSOLE, random :: RANDOM | eff)
type CounterDriver eff = Driver CounterQuery (console :: CONSOLE, random :: RANDOM | eff)

counterRunner :: forall eff b.
    String ->
    Aff (CounterEffects eff) b ->
    Aff (CounterEffects eff) (Tuple (Canceler (CounterEffects eff)) (CounterDriver eff))
counterRunner selector callback = do
    { node: node, driver: driver } <- runUI counterUI (initialCounterState (void callback))
    appendTo selector node
    i <- liftEff $ randomInt 100 700
    canceler <- forkAff $ setInterval i $ driver (action Tick)
    pure (Tuple canceler driver)
  where
    _log :: String -> Aff (console :: CONSOLE | eff) Unit
    _log s = if verbose then log s >>= \_ -> pure unit else pure unit

    setInterval :: forall a.
          Int ->
          Aff (CounterEffects eff) a ->
          Aff (CounterEffects eff) Unit
    setInterval ms action = later' ms $ do
        action
        setInterval ms action

counterMain :: forall eff a.
    String ->
    Aff (CounterEffects eff) a ->
    Eff (CounterEffects eff) Unit
counterMain selector callback = runAff throwException (const (pure unit))
    (counterRunner selector callback)

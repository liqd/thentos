module Main where

import Control.Monad.Eff
import Control.Monad.Eff.Console (CONSOLE(), log)
import Prelude

import qualified Register as Register
import qualified Data.Maybe as Data.Maybe


foreign import publish :: forall a. String -> String -> a -> forall eff. Eff eff Unit

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
    log "initializing thentos-purescript..."

{-
    publish "Main" "counter" Counter.counterMain
    publish "Main" "counter_" Counter.counterRunner
    publish "Main" "tick" (action Counter.Tick)
    publish "Main" "clear" (action Counter.Clear)
    publish "Main" "indicator" LoginIndicator.main
    publish "IFrames" "main" IFramesDemo.main
-}
    publish "Register" "main" Register.main
    publish "Register" "mainEl" Register.mainEl
    publish "Data.Maybe" "Just" Data.Maybe.Just
    publish "Data.Maybe" "Nothing" Data.Maybe.Nothing

    log "initialization of thentos-purescript complete!"

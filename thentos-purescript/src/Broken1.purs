-- build like this:
-- pulp browserify -m Broken1.purs --to ../static/thentos.js
module Broken1 where

import Control.Monad.Aff.Class (MonadAff)
import Control.Monad.Aff (Aff())
import DOM.HTML.Types (HTMLElement())
import Halogen (Component(), HalogenEffects(), Natural(), runUI, component)
import Prelude

import qualified Halogen.HTML.Indexed as H

data State = State
data Query a = Query a

ui :: forall eff g. (MonadAff (HalogenEffects eff) g) => Component State Query g
ui = component (\_ -> H.div_ []) (\(Query next) -> pure next)

main' :: forall eff a. (HTMLElement -> Aff (HalogenEffects eff) a) -> Aff (HalogenEffects eff) Unit
main' addToDOM = do
    { node: node, driver: driver } <- runUI ui State

    let driver' :: Natural Query (Aff (HalogenEffects eff))
        driver' = driver

    return unit

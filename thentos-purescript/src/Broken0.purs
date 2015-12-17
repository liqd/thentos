-- build like this:
-- pulp browserify -m Broken0.purs --to ../static/thentos.js
module Broken0 where

import Control.Monad.Aff.Class (MonadAff)
import Control.Monad.Aff (Aff())
import Data.Maybe
import DOM.HTML.Types (HTMLElement())
import Halogen (Component(), ComponentHTML(), ComponentDSL(), HalogenEffects(), Natural(), runUI, component, modify)
import Network.HTTP.Affjax (AJAX(), AffjaxResponse(), affjax, defaultRequest)
import Prelude

import qualified Data.ArrayBuffer.Types as AB
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.Query as Q

type Effs eff = HalogenEffects (ajax :: AJAX | eff)

type State = { stCaptchaQ :: Maybe (AffjaxResponse AB.ArrayBuffer) }

data Query a = NewCaptchaReceived (AffjaxResponse AB.ArrayBuffer) a

initialState :: State
initialState = { stCaptchaQ: Nothing }

render :: State -> ComponentHTML Query
render st = H.div_ []

-- fetchCaptcha :: forall eff. Natural Query (Aff (Effs eff)) -> Aff (Effs eff) Unit
fetchCaptcha driver = do
    response <- affjax defaultRequest
    driver (Q.action (NewCaptchaReceived response))

eval :: forall eff g. (MonadAff (Effs eff) g) => Natural Query (ComponentDSL State Query g)
eval e@(NewCaptchaReceived resp next) = do
    modify (_ { stCaptchaQ = Just resp })
    pure next

ui :: forall eff g. (MonadAff (Effs eff) g) => Component State Query g
ui = component render eval

main' :: forall eff a. (HTMLElement -> Aff (Effs eff) a) -> Aff (Effs eff) Unit
main' addToDOM = do
    { node: node, driver: driver } <- runUI ui initialState
    fetchCaptcha driver

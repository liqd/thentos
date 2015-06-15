module Thentos.Backend.Api.Docs.Simple where

import Data.Proxy (Proxy(Proxy))

import qualified Servant.Docs as Docs

import Thentos.Backend.Api.Docs.Common ()
import Thentos.Backend.Api.Simple (Api)

docs :: Docs.API
docs = Docs.docs (Proxy :: Proxy Api)

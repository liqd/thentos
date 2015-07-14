module Main where

import qualified ThentosDocs
import qualified Thentos.Adhocracy3.Backend.Api.Docs.Simple as Api


main :: IO ()
main = ThentosDocs.makeMain "./docs/generated" [("adhocracy3_proxy_mode", Api.docs)]

module Main where

import qualified ThentosDocs
import qualified Thentos.Adhocracy3.Backend.Api.Docs.Simple as Api


main :: IO ()
main = ThentosDocs.makeMain "./docs/generated" [("simple", Api.docs)]

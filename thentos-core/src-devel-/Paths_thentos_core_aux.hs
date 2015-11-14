module Paths_thentos_core_aux where

import System.Directory

import Language.Haskell.TH (Q, Exp, runIO)
import Language.Haskell.TH.Quote (dataToExpQ)

getBuildTimeCurrentDirectory :: Q Exp
getBuildTimeCurrentDirectory = runIO getCurrentDirectory >>= dataToExpQ (const Nothing)

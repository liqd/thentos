module Paths.TH (getBuildRootDirectory) where

import Language.Haskell.TH (Q, Exp, runIO)
import Language.Haskell.TH.Quote (dataToExpQ)
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)

getBuildRootDirectory :: Q Exp
getBuildRootDirectory =
    runIO (lookupEnv "THENTOS_BUILD_ROOT" >>= maybe getCurrentDirectory return)
      >>= dataToExpQ (const Nothing)

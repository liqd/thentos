{-# LANGUAGE TemplateHaskell #-}

module Thentos.Frontend.TH where

import Language.Haskell.TH (Q, Exp, runIO)
import Language.Haskell.TH.Quote (dataToExpQ)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import Paths_thentos_core (getDataFileName)

{-# NOINLINE staticContentRoot #-}
staticContentRoot :: FilePath
staticContentRoot = unsafePerformIO $ getDataFileName "frontend/static/"

loadStaticContent :: FilePath -> Q Exp
loadStaticContent filePath =
    runIO (readFile . (staticContentRoot </>) $ filePath) >>= dataToExpQ (const Nothing)

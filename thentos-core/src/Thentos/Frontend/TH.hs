{-# LANGUAGE TemplateHaskell #-}

module Thentos.Frontend.TH where

import System.FilePath ((</>))
import Language.Haskell.TH (Q, Exp, runIO)
import Language.Haskell.TH.Quote (dataToExpQ)

staticContentRoot :: FilePath
staticContentRoot = "frontend/static/"

loadStaticContent :: FilePath -> Q Exp
loadStaticContent filePath =
    runIO (readFile . (staticContentRoot </>) $ filePath) >>= dataToExpQ (const Nothing)

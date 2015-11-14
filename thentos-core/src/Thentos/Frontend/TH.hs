{-# LANGUAGE TemplateHaskell #-}

module Thentos.Frontend.TH where

import Language.Haskell.TH (Q, Exp, runIO)
import Language.Haskell.TH.Quote (dataToExpQ)
import System.FilePath ((</>))

import Paths_thentos_core__ (getBuildRootDirectory)

loadStaticContent :: FilePath -> Q Exp
loadStaticContent filePath =
    runIO (readFile ($(getBuildRootDirectory) </> "frontend/static/" </> filePath))
        >>= dataToExpQ (const Nothing)

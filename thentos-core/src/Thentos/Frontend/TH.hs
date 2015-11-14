{-# LANGUAGE TemplateHaskell #-}

module Thentos.Frontend.TH where

import Language.Haskell.TH (Q, Exp, runIO)
import Language.Haskell.TH.Quote (dataToExpQ)
import System.FilePath ((</>))

import Paths (getBuildRootDirectory)

loadStaticContent :: FilePath -> Q Exp
loadStaticContent filePath =
    runIO (readFile ($(getBuildRootDirectory) </> "thentos-core/frontend/static/" </> filePath))
        >>= dataToExpQ (const Nothing)

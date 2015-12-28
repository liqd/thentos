{-# LANGUAGE CPP             #-}
#if DEVELOPMENT
-- #warning "building in development mode (-fdevelopment)"
{-# LANGUAGE TemplateHaskell #-}
#else
-- #warning "building in production mode: (-f-development)"
#endif

-- | Custom modifications to the cabal-generated "Paths_thentos_core".
--
-- Quick motivation: we run code in at least the following different ways:
--
--     - the deployed binary.
--     - with hspec's sensei (via the `Makefile` rules in the git repo root).
--     - interactively (via the repl rules, same `Makefile`).
--     - the test suite (cabal `cabal test` or `./misc/thentos-install.hs`).
--     - via TH splices that run during compile time (e.g., to compile css source files as byte
--       strings into the executable)
--
-- In order to make sure the code will find places in the file system in all these contexts, the
-- cabal built-in functionality is almost enough, but not quite.  This file adds two little quirks.
--
-- 1. In development mode (cabal flag `development`), 'getDataFileName' returns the path into the
--    package root (it just calls 'getPackageSourceRoot').
-- 2. 'getPackageSourceRoot' is exported both from here and from "Paths.TH" for use in sibling
--    packages.
--
-- Related info: http://neilmitchell.blogspot.de/2008/02/adding-data-files-using-cabal.html
module Paths_thentos_core__ (getDataFileName, getPackageSourceRoot, version) where

import Paths.TH (getPackageSourceRoot)

#if !DEVELOPMENT
import Paths_thentos_core

#else
import Distribution.Version (Version(Version))
import System.FilePath ((</>))

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . ($(getPackageSourceRoot "thentos-core") </>)

version :: Version
version = Version [] []

#endif

{-# LANGUAGE CPP             #-}
#if DEVELOPMENT
{-# LANGUAGE TemplateHaskell #-}
#endif

-- | Custom modifications to the cabal-generated "Paths_thentos_adhocracy".  See analogous module in
-- thentos-core package for more information.
module Paths_thentos_adhocracy__ (getDataFileName, version) where

#if !DEVELOPMENT
import Paths_thentos_adhocracy

#else
import Distribution.Version (Version(Version))
import System.FilePath ((</>))

import Paths.TH (getPackageSourceRoot)

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . ($(getPackageSourceRoot "thentos-core") </>)

version :: Version
version = Version [] []

#endif

{-# LANGUAGE CPP             #-}
#if DEVELOPMENT
#warning "building in development mode (-fdevelopment)"
{-# LANGUAGE TemplateHaskell #-}
#else
#warning "building in production mode: (-f-development)"
#endif

-- | Custom modifications to "Paths_thentos_core".
module Paths_thentos_core__ (getDataFileName, getBuildRootDirectory, version) where

-- | use this only for testing or build-time effects!
import Paths.TH (getPackageSourceRoot)

#if !DEVELOPMENT
import Paths_thentos_core

#else
import Distribution.Version (Version(Version))
import System.FilePath ((</>))

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . ($(getPackageSourceRoot) </>)

version :: Version
version = Version [] []

#endif

{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Custom modifications to "Paths_thentos_core".
module Paths_thentos_core__ (getDataFileName, getBuildRootDirectory, version) where

-- | use this only for testing or build-time effects!
import Paths.TH (getBuildRootDirectory)

#if !DEVELOPMENT
import Paths_thentos_core

#else
import Distribution.Version (Version(Version))
import System.FilePath ((</>))

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . (($(getBuildRootDirectory) </> "thentos-core") </>)

version :: Version
version = Version [] []

#endif

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Paths (getDataFileName, getBuildRootDirectory, version) where

#if !DEVELOPMENT
import Paths_thentos_core

#else
import Distribution.Version (Version(Version))
import System.FilePath ((</>))

import Paths.TH (getBuildRootDirectory)

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . (($(getBuildRootDirectory) </> "thentos-core") </>)

version :: Version
version = Version [] []

#endif

-- | data files and development mode.  for details, see:
-- http://neilmitchell.blogspot.de/2008/02/adding-data-files-using-cabal.html
module Paths_thentos_core where

import System.FilePath

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . ("thentos-core" </>)

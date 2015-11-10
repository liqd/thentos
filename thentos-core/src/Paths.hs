{-# LANGUAGE CPP #-}
module Paths (getDataFileName, P.version) where

import qualified Paths_thentos_core as P

getDataFileName :: FilePath -> IO FilePath
#if DEVELOPMENT
getDataFileName x = return $ "thentos-core/" ++ x
#else
getDataFileName = P.getDataFileName
#endif

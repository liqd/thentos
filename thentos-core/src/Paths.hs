{-# LANGUAGE CPP #-}
module Paths (getDataFileName) where

#if !DEVELOPMENT
import Paths_thentos_core
#else
getDataFileName :: FilePath -> IO FilePath
getDataFileName x = return $ "thentos-core/" ++ x
#endif

{-# LANGUAGE ScopedTypeVariables #-}

module Paths.TH (getPackageSourceRoot) where

import Control.Exception (SomeException, catch)
import Data.Char (toUpper)
import Data.Maybe (catMaybes, listToMaybe)
import Language.Haskell.TH (Q, Exp, runIO)
import Language.Haskell.TH.Quote (dataToExpQ)
import System.Directory (getCurrentDirectory, canonicalizePath)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

-- | Takes a package name and returns a directory 'FilePath' at compile time.  The file path is
-- determined as follows (first working method wins):
--
-- 1. Shell variable.  Example: CABAL_PACKAGE_SOURCE_ROOT_THENTOS_CORE for package thentos-core.
-- 3. If current directory contains a directory with the same name as the package, take that.
-- 2. Current directory.
--
-- WARNING: use this only for testing or build-time effects!
getPackageSourceRoot :: FilePath -> Q Exp
getPackageSourceRoot fp =
    runIO (head . catMaybes <$> sequence
        [ lookupEnv (toShellVarName fp)
        , exceptToMaybe $ getCurrentDirectory >>= canonicalizePath . (</> fp)
        , Just <$> getCurrentDirectory
        ])
      >>= dataToExpQ (const Nothing)

exceptToMaybe :: IO a -> IO (Maybe a)
exceptToMaybe a = (Just <$> a) `catch` \(_ :: SomeException) -> return Nothing

toShellVarName :: FilePath -> FilePath
toShellVarName fp = "CABAL_PACKAGE_SOURCE_ROOT_" ++ (f <$> fp)
  where
    f '-' = '_'
    f c = toUpper c

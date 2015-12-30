module Paths.TH (getPackageSourceRoot) where

import Control.Exception (SomeException(SomeException), catch)
import Data.Char (toUpper)
import Data.Maybe (catMaybes)
import Language.Haskell.TH (Q, Exp, runIO)
import Language.Haskell.TH.Quote (dataToExpQ)
import System.Directory (getCurrentDirectory, canonicalizePath)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

-- | Takes a package name and returns a directory 'FilePath' at compile time.  The file path is
-- determined as follows (first working method wins):
--
-- 1. Shell variable.  Example: CABAL_PACKAGE_SOURCE_ROOT_THENTOS_CORE for package thentos-core.
-- 2. If current directory contains a directory with the same name as the package, take that.
-- 3. Like 2., but on *parent* directory.
-- 4. Take current directory.
--
-- WARNING: use this only for testing or build-time effects!
getPackageSourceRoot :: FilePath -> Q Exp
getPackageSourceRoot fp =
    runIO (head . catMaybes <$> sequence
        [ lookupEnv (toShellVarName fp)
        , perhaps fp
        , perhaps $ ".." </> fp
        , Just <$> getCurrentDirectory
        ])
      >>= dataToExpQ (const Nothing)

perhaps :: FilePath -> IO (Maybe FilePath)
perhaps fp = exceptToMaybe $ getCurrentDirectory >>= canonicalizePath . (</> fp)

exceptToMaybe :: IO a -> IO (Maybe a)
exceptToMaybe a = (Just <$> a) `catch` \(SomeException _) -> return Nothing

toShellVarName :: FilePath -> FilePath
toShellVarName fp = "CABAL_PACKAGE_SOURCE_ROOT_" ++ (f <$> fp)
  where
    f '-' = '_'
    f c = toUpper c

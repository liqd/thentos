#!/usr/bin/env runhaskell

module Main where

import Control.Concurrent.MVar (newMVar, modifyMVar, readMVar)
import Control.Monad (when, unless, forM_)
import Data.List (intercalate, nub, (\\))
import System.Directory
    (getCurrentDirectory, setCurrentDirectory, doesFileExist, canonicalizePath)
import System.Environment (getEnv, getArgs, getProgName)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)
import System.FilePath ((</>), takeDirectory, splitPath)
import System.IO (hPutStrLn, stderr)
import System.Process (system, readProcess)

import qualified Data.ByteString as SBS (readFile, take, drop, findIndices)
import qualified Data.ByteString.Char8 as SBS (unpack)


printSectionHeading :: String -> IO ()
printSectionHeading = hPutStrLn stderr . ("\n\n" ++) . (++ "...\n")

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir cmd = do
    previous <- getCurrentDirectory
    setCurrentDirectory dir
    r <- cmd
    setCurrentDirectory previous
    return r

main :: IO ()
main = do
    args <- getCliArgs
    rootPath <- changeToProjectRoot
    exitCode <- newMVar ExitSuccess

    let storeResult :: ExitCode -> IO ()
        storeResult e = modifyMVar exitCode $ \e' -> return (max e e', ())

    gitSubmodules
    cabalSandbox rootPath
    buildPurescript args >>= storeResult

    printSectionHeading "building dependencies"
    ExitSuccess <- runCabal args "--dependencies-only"
    -- FIXME: this will build thentos-core, thentos-test, and servant-session,
    -- as they are all dependencies of thentos-adhocracy.  not sure how to work
    -- around that, since we want to distinguish between deps (no -Werror) and
    -- targets (-Werror).  the only bad thing about the status quo is that those
    -- packages twice are compiled twice.

    unless (CliArgDepsOnly `elem` args) $ do
          printSectionHeading "building thentos-* packages"
          if not (CliArgThorough `elem` args)
              then do
                  runCabal args "" >>= storeResult
              else do
                  forM_ sources $ \s -> do
                      withCurrentDirectory (rootPath </> s) $ do
                          ExitSuccess <- system "cabal clean"
                          return ()
                  system "make hlint" >>= storeResult
                  runCabal args "--ghc-options=-Werror" >>= storeResult
                  forM_ sources $ \s -> do
                      printSectionHeading $ "searching for tests in " ++ s
                      withCurrentDirectory (rootPath </> s) $ do
                          ec <- system $ "grep -q ^test-suite " ++ s ++ ".cabal"
                          when (ec == ExitSuccess)
                              (system "cabal test" >>= storeResult)

    readMVar exitCode >>= exitWith


-- * config section

submoduleSources :: [FilePath]
submoduleSources = map ("submodules" </>)
    [ "servant/servant"
    , "servant/servant-server"
    , "servant/servant-client"
    , "servant/servant-docs"
    , "servant/servant-blaze"
    , "servant/servant-js"
    , "servant/servant-foreign"
    , "pronk"
    ]

sources :: [FilePath]
sources =
    [ "servant-session"
    , "thentos-core"
    , "thentos-tests"
    , "thentos-adhocracy"
    ]

sandboxPath :: FilePath
sandboxPath = ".cabal-sandbox"


-- * cli parser

data CliArg =
      CliArgCabalArgs String
    | CliArgNoPurescript
    | CliArgDepsOnly
    | CliArgThorough
  deriving (Eq, Show, Ord)

getCliArgs :: IO [CliArg]
getCliArgs = do
    raw <- getArgs

    when ("-h" `elem` raw) $ do
        usageInfo ""
    when (nub raw /= raw) $ do
        usageInfo $ "duplicate params: " ++ show raw
    case raw \\ ["-h", "-p", "-d", "-t"] of
        []        -> return ()
        ["-c", _] -> return ()
        bad -> usageInfo $ "superflous params: " ++ show bad

    let args =
          [ CliArgNoPurescript | "-p" `elem` raw ] ++
          [ CliArgDepsOnly     | "-d" `elem` raw ] ++
          [ CliArgThorough     | "-t" `elem` raw ] ++
          (map CliArgCabalArgs $ f raw)

        f :: [String] -> [String]
        f [] = []
        f ("-c":s:_) = [s]
        f (_:xs) = f xs

    hPutStrLn stderr $ "cli args: " ++ show args
    return args

usageInfo :: String -> IO ()
usageInfo msg = do
    hPutStrLn stderr . unlines $
        (msg ++ "\n") :
        "usage: thentos-install.hs [-c <CABAL-OPTS>] [-p] [-d] [-t]" :
        "  Installs thentos packages and their dependencies into a cabal sandbox." :
        "  '-p' means 'do not build purescript'." :
        "  '-d' means 'dependencies only'.  cancels out '-t'." :
        "  '-t' means 'thorough' (compiles with -Werror, runs hlint, test suite)." :
        []
    exitWith $ if null msg
        then ExitSuccess
        else ExitFailure 1


-- * find current directory

-- | (Only supports Linux, MacOSX.)
changeToProjectRoot :: IO FilePath
changeToProjectRoot = do
    osfpr <- readProcess "uname" [] ""
    case osfpr of
        "Linux\n" -> changeToProjectRootLinux
        "Darwin\n" -> changeToProjectRootMacOSX
        bad -> error $ "unknown os footprint (`uname`): " ++ show bad

-- | (This is a bit hacky, but it seems to work on Linux.)
changeToProjectRootLinux :: IO FilePath
changeToProjectRootLinux = do
    cmdline <- SBS.readFile "/proc/self/cmdline"
    let raw :: String
        raw = case take 2 . reverse . SBS.findIndices (== 0) $ cmdline of
                [rear, front] -> SBS.unpack
                               . SBS.take (rear - front - 1)
                               . SBS.drop (front + 1)
                               $ cmdline
    it <- canonicalizePath $ takeDirectory raw </> ".."
    setCurrentDirectory it
    hPutStrLn stderr $ "working directory: " ++ show it
    return it

-- | (This is even hackier.  OS/X people are welcome to fix this.)
changeToProjectRootMacOSX :: IO FilePath
changeToProjectRootMacOSX = do
    hPutStrLn stderr $ "NOTE: only call this from git repo base directory!"
    "thentos" <- last . splitPath <$> (getCurrentDirectory >>= canonicalizePath)
    getCurrentDirectory


-- * make rules

gitSubmodules :: IO ()
gitSubmodules = do
    checkSync <- system "git status --porcelain | egrep -q '^\\s*M submodules'"
    when (checkSync == ExitSuccess) $ do
        ExitSuccess <- system "git submodule sync"
        return ()
    ExitSuccess <- system "git submodule update --init"
    return ()

cabalSandbox :: FilePath -> IO ()
cabalSandbox rootPath = do
    ExitSuccess <- system "test -e cabal.sandbox.config || cabal sandbox init"
    forM_ (submoduleSources ++ sources) $ \s -> do
        withCurrentDirectory (rootPath </> s) $ do
            doesFileExist "cabal.sandbox.config" >>= \yes -> unless yes $ do
                ExitSuccess <- system $ "cabal sandbox init --sandbox="
                                                ++ (rootPath </> sandboxPath)
                ExitSuccess <- system "cabal sandbox add-source ."
                return ()

buildPurescript :: [CliArg] -> IO ExitCode
buildPurescript args = if CliArgNoPurescript `elem` args
    then return ExitSuccess
    else do
        printSectionHeading "building thentos-purescript"
        e1 <- system "./thentos-purescript/build.sh pull-cache $HOME/.th-psc-cache"
        e2 <- system "./thentos-purescript/build.sh dep"
        e3 <- system "./thentos-purescript/build.sh it"
        e4 <- system "./thentos-purescript/build.sh push-cache $HOME/.th-psc-cache"
        return $ maximum [e1, e2, e3, e4]

runCabal :: [CliArg] -> String -> IO ExitCode
runCabal args extraArgs = do
    let cmd = intercalate " " $
                  [ "cabal install"
                  , userArgs, extraArgs
                  , "--enable-tests", "--enable-bench"
                  , "--max-backjumps -1", "--reorder-goals"
                  , "-fwith-thentos-executable"
                  , "-fwith-captcha-executable"
                        -- (these should only be here for thentos-core source)
                  ] ++ sources

        userArgs = concat $ fmap f args
          where f (CliArgCabalArgs s) = s
                f _ = ""

    hPutStrLn stderr $ ">>> " ++ cmd
    system cmd

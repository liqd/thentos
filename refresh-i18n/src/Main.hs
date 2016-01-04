{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | usage: "$0 [target_path]"
--
-- (I tried to use packages purescript, language-javascript, and language-purescript, but it was a
-- lot easier to do just stupid pattern matching on the source files without any parsing.)
module Main
where

import Control.Exception (assert)
import Control.Monad (void)
import Data.Function (on)
import Data.Functor.Infix ((<$$>))
import Data.List (foldl', union, sort, nub, groupBy)
import Data.Maybe (listToMaybe, catMaybes)
import Data.Monoid ((<>))
import Data.String.Conversions (ST, cs)
import System.Directory (doesDirectoryExist, setCurrentDirectory, getDirectoryContents)
import System.Environment (lookupEnv, getArgs)
import System.FilePath ((</>), (<.>), takeExtension)
import System.IO (hPutStrLn, stderr)
import System.Process (system)

import qualified Data.Text as ST
import qualified Data.Text.IO as ST


i18nModule :: FilePath
i18nModule = "I18n"

main :: IO ()
main = do
    setCurrentDirectoryToTarget
    purss :: [FilePath] <- getTranslateableFiles "."
    transKeys :: [ST] <- nub . sort . foldl' union [] <$> mapM translationKeys purss
    !(tablesModule :: ST) <- ST.readFile (i18nModule <.> "js")
    ST.writeFile (i18nModule <.> "js-") $ updateKeys transKeys tablesModule
    void $ system ("mv " ++ (i18nModule <.> "js-") ++ " " ++ (i18nModule <.> "js"))


setCurrentDirectoryToTarget :: IO ()
setCurrentDirectoryToTarget = do
    mEnvVar <- (</> "thentos-purescript" </> "src") <$$> lookupEnv "THENTOS_DEV_ROOT"
    mArgPath <- do
        args <- getArgs
        case args of
            [fp] -> (\yes -> if yes then Just fp else Nothing) <$> doesDirectoryExist fp
            [] -> pure Nothing

    let Just target = listToMaybe . catMaybes $ [mArgPath, mEnvVar, Just "."]
    hPutStrLn stderr $ "target directory: " ++ show target
    setCurrentDirectory target


getTranslateableFiles :: FilePath -> IO [FilePath]
getTranslateableFiles = (filter f <$>) . getDirectoryContents
  where
    f fp = (fp /= i18nModule <.> "purs") && takeExtension fp == ".purs"


-- | Load a text file and collect all text literals of the form "TR__...".  Return a sorted, nubbed
-- list of all those texts.
--
-- Translation keys must occur as intact string literals (@"TR__" <$> ["A", "B"]@ is not allowed).
-- They must only consist of capitals and underscores (in particular, double quotes won't work.)
translationKeys :: FilePath -> IO [ST]
translationKeys purs = f <$> ST.readFile purs
  where
    f raw = case ST.breakOn "\"TR__" raw of
        (_, "") -> []
        (_, next) -> g $ ST.tail next

    g hit = case ST.breakOn "\"" hit of
        (key, rest) -> key : f (ST.tail rest)


-- | Remove all inactive entires in dict; add missing entires into dict with "TODO" as translation.
updateKeys :: [ST] -> ST -> ST
updateKeys activeKeys = unGrp . (checkGrp <$>) . grp
  where
    grp :: ST -> [[ST]]
    grp = groupBy ((==) `on` ("\"TR__" `ST.isInfixOf`)) . ST.lines

    unGrp :: [[ST]] -> ST
    unGrp = ST.unlines . concat

    checkGrp :: [ST] -> [ST]
    checkGrp [] = []
    checkGrp noise@(h:_) | not ("\"TR__" `ST.isInfixOf` h) = noise
    checkGrp table = (renderLine <$>) . refreshGrp . (parseLine <$>) $ table

    refreshGrp :: [(ST, ST, ST)] -> [(ST, ST, ST)]
    refreshGrp orig = completed
      where
        fst3 (k, _, _) = k
        pruned = filter ((`elem` activeKeys) . fst3) orig
        completed = pruned ++
            [ (key, "    ", ": @@@") | key <- activeKeys, not (key `elem` (fst3 <$> pruned)) ]

    parseLine :: ST -> (ST, ST, ST)
    parseLine l = case ST.breakOn "\"TR__" l of
        (pre, ST.tail -> rest) -> case ST.findIndex (== '"') rest of
            Nothing -> assert False $ error "updateKeys: parse error in translation rule"
            Just i -> case ST.splitAt i rest of
                (key, ST.tail -> post) -> (key, pre, post)

    renderLine :: (ST, ST, ST) -> ST
    renderLine (key, pre, post) = pre <> cs (show key) <> post

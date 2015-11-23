{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | To generate and publish new docs, make sure your working copy is clean, chdir to the root of
-- the git repo, and do this:
--
-- >>> ./misc/thentos-install.sh
-- >>> cabal exec -- ghc --make -main-is Doc misc/build-docs/Doc.hs
-- >>> git checkout gh-pages
-- >>> misc/build-docs/Doc
--
-- Then inspect the diff, and add/commit in order to publish if happy.
module Doc
where

import Control.Monad (void)
import Data.Char (toUpper)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, (<>), cs)
import Servant.Docs (markdown)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))
import System.IO (hGetContents)
import System.Process (runInteractiveCommand, system)

import qualified Data.Text as ST
import qualified Data.Text.IO as ST
import qualified Thentos.Adhocracy3.Backend.Api.Simple as A
import qualified Thentos.Backend.Api.Docs.Common as TD
import qualified Thentos.Backend.Api.Simple as S


main :: IO ()
main =
  do
    assertGitBranch "gh-pages"
    putStrLn "Generating documentation and client code..."

    writeDocs "core" (Proxy :: Proxy (TD.RestDocs S.Api))
    writeDocs "adhocracy3" (Proxy :: Proxy (TD.RestDocs A.Api))


writeDocs :: forall api. TD.HasFullDocExtras api => FilePath -> Proxy (TD.RestDocs api) -> IO ()
writeDocs stem proxy = do
    let path = "./gh-pages/servant-docs"

    createDirectoryIfMissing True path
    writeFile (path </> stem <.> "md") . markdown $ TD.restDocsMd proxy
    void . system $ "pandoc " ++ (path </> stem <.> "md") ++ " -o " ++ (path </> stem <.> "html")
    void . system $ "pandoc " ++ (path </> stem <.> "md") ++ " -o " ++ (path </> stem <.> "docx")

    let path' = path </> "client-code"
        proxy' :: Proxy api = Proxy

    createDirectoryIfMissing True path'
    let fp = path' </> stem <> "_vanilla" <.> "js"
      in ST.writeFile fp $ TD.restDocsJs proxy
    let fp = path' </> stem <> "_ng" <.> "js"
      in ST.writeFile fp $ TD.restDocsNg proxy

    let fp :: FilePath = path' </> cs moduleName <.> "purs"
        moduleName :: ST = capitalise $ cs stem
      in ST.writeFile fp $ TD.restDocsPurs proxy moduleName


-- | this is defined in servant-foreign, but not exported as of today.  watch
-- https://github.com/haskell-servant/servant/pull/265.
capitalise :: ST -> ST
capitalise ""   = ""
capitalise name = toUpper (ST.head name) `ST.cons` ST.tail name


assertGitBranch :: String -> IO ()
assertGitBranch branch = isCurrentGitBranch branch >>= \b -> if b
    then return ()
    else error $ "assertGitBranch: not on " ++ show branch

isCurrentGitBranch :: String -> IO Bool
isCurrentGitBranch (("On branch " <>) -> pattern) = do
    (_, o, _, _) <- runInteractiveCommand "git status"
    ((pattern `elem`) . lines) <$> hGetContents o

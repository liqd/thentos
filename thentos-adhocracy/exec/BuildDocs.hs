module Main where

import Control.Applicative ((<$>), (<*>))
import Data.Char (toLower)
import Data.Thyme.Time ()
import Servant.Docs.Pandoc (pandoc)
import System.Directory (setCurrentDirectory)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((<.>))
import System.Process (system)
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Text.Pandoc (writeMarkdown, writeHtml, writeDocx, def)

import qualified Data.ByteString.Lazy as LBS
import qualified Servant.Docs as Docs

import Thentos.Backend.Api.Docs.Common (prettyMimeRender)
import qualified Thentos.Adhocracy3.Backend.Api.Docs.Simple as A3


targetPath :: FilePath
targetPath = "./docs/generated/"

main :: IO ()
main = do
    xsystem $ "mkdir -p " ++ targetPath
    setCurrentDirectory targetPath
    sequence_ $ xwriter <$> [minBound..] <*> [minBound..]

xwriter :: ApiName -> FormatName -> IO ()
xwriter apiName formatName = do
    let fileName = map toLower $ show apiName
        doc = pandoc $ xdocs apiName
    case formatName of
        Format_Markdown
            -> writeFile (fileName <.> "md") (writeMarkdown def doc)
        Format_Html
            -> LBS.writeFile (fileName <.> "html") (renderMarkup $ writeHtml def doc)
        Format_Docx
            -> writeDocx def doc >>= LBS.writeFile (fileName <.> "docx")

-- FIXME: this crashes for poorly understood reasons:
--
--        Format_Pdf
--            -> do
--                 makePDF "pdflatex" writeLaTeX def doc >>=
--                     \case (Right pdf) -> LBS.writeFile (fileName <.> "pdf") pdf
--                           (Left bad) -> error $ show bad

xdocs :: ApiName -> Docs.API
xdocs api = prettyMimeRender docs
  where
    docs = case api of
        Api_Adhocracy3 -> A3.docs

data ApiName = Api_Adhocracy3
  deriving (Eq, Enum, Bounded, Read, Show)

data FormatName = Format_Markdown | Format_Html | Format_Docx
  deriving (Eq, Enum, Bounded, Read, Show)

xsystem :: String -> IO ()
xsystem cmd = do
    putStrLn $ ">> " ++ cmd
    ExitSuccess <- system cmd
    return ()

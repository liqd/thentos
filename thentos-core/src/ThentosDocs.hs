module ThentosDocs (makeMain) where

import Data.Thyme.Time ()
import Servant.Docs (API, markdown)
import System.Directory (createDirectory, setCurrentDirectory)
import System.FilePath ((<.>))


makeMain :: FilePath -> [(String, API)] -> IO ()
makeMain targetPath apis = do
    createDirectory targetPath
    setCurrentDirectory targetPath
    mapM_ (\ (apiName, api) -> writeFile (apiName <.> "md") (markdown api)) apis

module ThentosDocs (makeMain) where

import Servant.Docs (API, markdown)
import System.Directory (createDirectoryIfMissing, setCurrentDirectory)
import System.FilePath ((<.>))


makeMain :: FilePath -> [(String, API)] -> IO ()
makeMain targetPath apis = do
    createDirectoryIfMissing True targetPath
    setCurrentDirectory targetPath
    mapM_ (\ (apiName, api) -> writeFile (apiName <.> "md") (markdown api)) apis

import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, postConf)
import System.Directory (setCurrentDirectory)

import System.Process (callProcess)

buildJs :: IO ()
buildJs = do
    setCurrentDirectory "./purescript"
    callProcess "./build-js.sh" []
    return ()

main :: IO ()
main = do
    defaultMainWithHooks $ simpleUserHooks { postConf = \_ _ _ _ -> buildJs }

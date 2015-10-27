import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, postConf)
import System.Process (callProcess)

buildJs :: IO ()
buildJs = do
    callProcess "./purescript/build-js.sh" []
    return ()

main :: IO ()
main = do
    defaultMainWithHooks $ simpleUserHooks { postConf = \_ _ _ _ -> buildJs }

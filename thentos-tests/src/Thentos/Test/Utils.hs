{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
module Thentos.Test.Utils where

import Data.Configifier (configify, Source(YamlString))
import Data.ByteString.Char8 (pack)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (runQ)
import System.IO.Unsafe (unsafePerformIO)


-- | QuasiQuoter for config files.
cfgify :: QuasiQuoter
cfgify = QuasiQuoter { quoteExp = \x -> runQ [| unsafePerformIO $ configify [YamlString $ pack x] |]
                     , quotePat = error "unimplemented"
                     , quoteType = error "unimplemented"
                     , quoteDec = error "unimplemented"
                     }

module Thentos.Test.Utils where

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (Exp(LitE), stringL)

-- | QuasiQuoter for multiline string literals
strLit :: QuasiQuoter
strLit = QuasiQuoter { quoteExp = return . LitE . stringL
                     , quotePat = error "unimplemented"
                     , quoteType = error "unimplemented"
                     , quoteDec = error "unimplemented"
                     }

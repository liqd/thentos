import "hint" HLint.Default
import "hint" HLint.Dollar
import "hint" HLint.Generalise
import "hint" HLint.HLint
import "hint" HLint.Test

ignore "Use list literal"
ignore "Redundant do" = TestMain


-- FIXME: can i find / write a lint rule that disallows
-- -fdefer-type-errors in OPTIONS pragmas?

-- FIXME: check all modules for ghc options and move things to cabal
-- file if appropriate.

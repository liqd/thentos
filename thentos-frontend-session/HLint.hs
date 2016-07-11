import "hint" HLint.Default
import "hint" HLint.Dollar
import "hint" HLint.Generalise
import "hint" HLint.HLint

-- how to tweak the rules: http://community.haskell.org/~ndm/darcs/hlint/hlint.htm

ignore "Redundant lambda" = Thentos.Backend.Api.Simple
ignore "Redundant bracket" = Thentos.Backend.Api.Simple
ignore "Redundant $"
ignore "Redundant do"
ignore "Use ."
ignore "Use camelCase"
ignore "Use const"
ignore "Use fmap"
ignore "Use head"
ignore "Use list literal"
ignore "Use mappend"
ignore "Use record patterns"
ignore "Parse error"

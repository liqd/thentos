import "hint" HLint.Default
import "hint" HLint.Dollar
import "hint" HLint.Generalise
import "hint" HLint.HLint

-- some rules are disabled universally; some are ignored in some modules.  syntax:
--
-- >>> rule    ::= 'ignore' pattern [= module]
-- >>> module  ::= 'Thentos.Backend.Api.Adhocracy3'
-- >>> pattern ::= '"' string '"'

ignore "Reduce duplication" = Thentos.FrontendSpec
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

-- FIXME: missing checks:
--
--  - can i find / write a lint rule that disallows -fdefer-type-errors in OPTIONS pragmas?
--  - check all modules for ghc options and move things to cabal file if appropriate.
--  - language extensions enabled in cabal file should not be re-enabled in modules.

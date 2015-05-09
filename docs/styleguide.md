# List of programming style rules

(will be re-structured as it grows)


## imports

FIXME: single-letter module names are also allowed (as is tradition).  is there a rule in this at all?

qualified imports are given the name of the module (without path),
not an abbreviation.  good:

```haskell
import qualified Data.Map as Map
```

  bad:

```haskell
import qualified Data.Map as M
```

obviously, this rule sometimes needs bending.  bad:

```haskell
import qualified Data.HashMap.Strict as Strict
```

  good:

```haskell
import qualified Data.HashMap.Strict as HashMap
```

  further exceptions:

```haskell
import Data.ByteString as SBS
import Data.ByteString.Lazy as LBS
import Data.Text as ST
import Data.Text.Lazy as LT
```

(FIXME: `LT` is `less than` from prelude.  is there a better name?  (should string-conversions adopt that, too?))

## naming

Acronyms in names are camelized.  For instance: `ThreadId`, not
`ThreadID`; `Thentos.Db` and `Thentos.Smtp`, not `DB` or `SMTP`.

Names for `Either` values start with an `e` (example: `eSession`).
Analogously, `Maybe` value names start with an `m`.


## string types

type synonyms SBS, LBS, ST, LST from string-conversions are used in
type signatures (even if no strings are converted).


### rationale

Commonly used types like `Text` and `ByteString` are ambigous (strict
or lazy).  Also, these short cuts follow a logical pattern, are very
compact, and occur often enough in this code base (and in every other
that makes use of `string-conversions`, such as e.g. `servant`) for
even occasional readers to get used to them.  Also, these type
synonyms match the module names that they are from.


## layout

maximum line length is 140 chars.

preceed haddock section headings with two empty lines.

spacing in lambda: `\ x -> ...`; NOT: `\x -> ...`.

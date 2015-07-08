# List of programming style rules

(Will be re-structured as it grows.)


## Testing

### HLint

There is an hlint rule in the Makefile, and an associated customization
file HLint.hs.  This implements some elements of the automatable part of
the coding policy.


### hspec module names

For each file of the form `src/*.hs`, there may be a file called
`tests/*Spec.hs` that contains tests for the code in this file.  These
are automatically collected by hspec-discover.

Other test modules, or tests for code from unassociated production
modules, are only allowed if there are good reasons.  These reasons
must be documented in the module's haddock comment.


## Imports

Import statements are sorted in up to four groups. Imports within each
group should be sorted alphabetically and groups should be separated by a
single line.

 1. Explicit unqualified imports from third-party modules (including the
    base library). Imported functions and types **must** be listed
    explicitly.
 2. Qualified imports from third-party modules. Rename them using,
    preferably, the name of the module (without path). If that would result
    in a non-descriptive name such as `Strict`, use an earlier part of the
    full name (see the `Map` and `URI` samples below). Import Text and
    ByteString modules using the names defined in the string-conversions
    package (`SBS, LBS, ST, LT`). It's OK to rename several related modules
    to the same name if their definitions don't overlap (see the `Aeson`
    example below). Other abbreviations should generally be avoided.
 3. Unqualified imports from Thentos. It's OK to omit the explicit import
    list in this case.
 4. Qualified imports from Thentos. Here it's acceptable to use
    abbreviations (typically one or two letters) when renaming them.

```haskell
import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad (unless)
import Data.String.Conversions (SBS, ST, cs)
import Text.Show.Pretty (ppShow)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as SBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as ST
import qualified Data.Thyme as Thyme
import qualified URI.ByteString as URI

import Thentos.Action.Core
import Thentos.Config
import Thentos.Types

import qualified Thentos.Frontend.Handlers as H
import qualified Thentos.Frontend.Pages as P
```


## Naming

Acronyms in names are camelized.  For instance: `ThreadId`, not
`ThreadID`; `Thentos.Db` and `Thentos.Smtp`, not `DB` or `SMTP`.

Names for `Either` values start with an `e` (example: `eSession`).
Analogously, `Maybe` value names start with an `m`.


## String types

Type synonyms SBS, LBS, ST, LT from string-conversions are used in type
signatures (even if no strings are converted).


### Rationale

Commonly used types like `Text` and `ByteString` are ambiguous (strict
or lazy).  Also, these short cuts follow a logical pattern, are very
compact, and occur often enough in this code base (and in every other
that makes use of `string-conversions`, such as e.g. `servant`) for
even occasional readers to get used to them.  Also, these type
synonyms match the module names that they are from.


## Layout

Maximum line length is 100 chars.

Precede haddock section headings with two empty lines.  Succeed with
one empty line.

Spacing in lambda: `\x -> ...`; NOT: `\ x -> ...`.

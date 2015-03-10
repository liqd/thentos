
List of programming style rules
===============================

(will be re-structured as it grows)

- qualified imports are given the name of the module (without path),
  not an abbreviation.  good:

    import qualified Data.Map as Map

  bad:

    import qualified Data.Map as M

  obviously, this rule sometimes needs bending.  bad:

    import qualified Data.HashMap.Strict as Strict

  good:

    import qualified Data.HashMap.Strict as HashMap

  further exceptions:

    import Data.ByteString as SBS
    import Data.ByteString.Lazy as LBS
    import Data.Text as ST
    import Data.Text.Lazy as LT

- type synonyms SBS, LBS, ST, LST from string-conversions are used in
  type signatures (even if no strings are converted).

  Rationale: Commonly used types like `Text` and `ByteString` are
  ambigous (strict or lazy).  Also, these short cuts follow a logical
  pattern, are very compact, and occur often enough in this code base
  (and in every other that makes use of `string-conversions`, such as
  e.g. `servant`) for even occasional readers to get used to them.
  Also, these type synonyms match the module names that they are from.

- maximum line length is 140 chars.

- preceed haddock section headings with two empty lines.

- `Either` values are named `e`.  Example: `eSession`.  Analogously,
  `Maybe` values start with an `m`.

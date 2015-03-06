
List of programming style rules
===============================

(will be re-structured as it grows)

- qualified imports are given the name of the module (without path),
  not an abbreviation (`import qualified Data.Map as Map`).

- exceptions:

    import Data.ByteString as SBS
    import Data.ByteString.Lazy as LBS
    import Data.Text as ST
    import Data.Text.Lazy as LT

- type synonyms SBS, LBS, ST, LST from string-conversions are used in
  type signatures (even if no strings are converted).

- maximum line length is 140 chars.

- preceed haddock section headings with two empty lines.

- either values are named `eSession`; maybe values `mSession`.

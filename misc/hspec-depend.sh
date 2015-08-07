#!/bin/sh

# This generates an extra-source-files entry for a cabal file in
# the current directory, to fix dependency tracking when using
# hspec-discover.

echo extra-source-files:
find . -type f | grep -E '[a-zA-Z0-9]Spec\.l?hs' | sed -e 's/^../  /'

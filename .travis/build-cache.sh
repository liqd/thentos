#!/bin/bash

test "$1" == "--yes-i-know" || exit 1
test "`pwd`" == "/home/travis/thentos" || exit 2
test "`whoami`" == "travis" || exit 3

# clone this repo into /home/travis/thentos, and run this script
# from there like this:
#
#     ./.travis/build-cache.sh --yes-i-know
#
# WARNING: THIS SCRIPT IS NOT TESTED VERY THOROUGHLY!
# ALSO IT REMOVES ~/.cabal, ~/.ghc!

rm -rf ~/.cabal ~/.ghc
cabal update
time cabal install \
    --dependencies-only --enable-tests --disable-documentation \
  || exit 1

cd ~
time tar cvpJf new-cache.tar.xz .cabal .ghc || exit 1
export FNAME=`sha1sum new-cache.tar.xz | perl -ne '/^(\S+) / && print "$1"'`
mv new-cache.tar.xz $FNAME.tar.xz || exit 1

echo 'now move ~travis/$FNAME.tar.xz to where travis can find it.'
echo '(don'\''t forget to update hash in the install rule in .travis.yml!)'

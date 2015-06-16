#!/bin/bash

test "`pwd`" == "/home/travis/build/liqd/thentos" || exit 2

# clone this repo to /home/travis/build/liqd/thentos, and run this script
# from there like this:
#
#     ./.travis/build-cache.sh
#
# WARNING: THIS SCRIPT IS NOT TESTED VERY THOROUGHLY!

cabal update
cabal sandbox delete
cabal sandbox init
time cabal install \
    --dependencies-only --enable-tests --disable-documentation \
  || exit 1

time cabal install hlint || exit 1

time tar cvpJf new-cache.tar.xz .cabal-sandbox || exit 1

echo 'now move /home/travis/build/liqd/thentos/new-cache.tar.xz to where travis can find it.'

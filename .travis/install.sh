#!/bin/bash

export CACHE_URL="https://github.com/liqd/travis-cache/raw/master/new-cache.tar.xz"
export VERBOSE=1

function fallback() {
    echo $1
    if [ "$VERBOSE" == "1" ]; then
        echo "=== cabal --version"  ; cabal --version
        echo "=== cabal.config:"    ; cat cabal.config
        echo "=== ~/.cabal/config:" ; cat ~/.cabal/config
        echo "=== ghc-pkg list"     ; ghc-pkg list
    fi
    cabal update
    cabal install --only-dependencies --enable-tests --disable-documentation
    exit 0
}

function recover_cache() {
    echo "recovering cache from ${CACHE_URL}..."
    wget "${CACHE_URL}" || fallback "could not fetch cache"
    echo "Unpack: start unpacking sandbox"
    tar xpJ -f new-cache.tar.xz || fallback "could not unpack cache"
    echo "Unpack: done."
    cabal sandbox init
    rm new-cache.tar.xz
}

recover_cache
fallback "updating cache"

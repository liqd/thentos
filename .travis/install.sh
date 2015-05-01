#!/bin/bash

export ARCHIVE_HASH=$1
export CACHE_URL="https://github.com/liqd/travis-cache/raw/master/thentos/${ARCHIVE_HASH}.tar.xz"
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
    echo "${ARCHIVE_HASH}  ${ARCHIVE_HASH}.tar.xz" | sha1sum -c || fallback "bad checksum"
    echo "Unpack: rm -rf ~/.{cabal,ghc}..."
    rm -rf ~/.{cabal,ghc}
    echo "Unpack: start unpacking to ~/..."
    tar xpJ -f ${ARCHIVE_HASH}.tar.xz -C ~ || fallback "could not unpack cache"
    echo "Unpack: done."
    rm ${ARCHIVE_HASH}.tar.xz
}

recover_cache
fallback "updating cache"

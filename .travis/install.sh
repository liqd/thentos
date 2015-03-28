#!/bin/bash

export SANDBOX_HASH=$1
export CACHE_URL="https://github.com/liqd/travis-cache/raw/master/thentos/${SANDBOX_HASH}.tar.xz"

function fallback() {
    echo $1
    cabal update
    cabal install --only-dependencies --enable-tests --disable-documentation
    exit 0
}

function recover_cache() {
    echo "recovering cache from ${CACHE_URL}..."
    wget "${CACHE_URL}" || fallback "could not fetch cache"
    echo "${SANDBOX_HASH}  ${SANDBOX_HASH}.tar.xz" | sha1sum -c || fallback "bad checksum"
    tar xpJ -f ${SANDBOX_HASH}.tar.xz -C ~ || fallback "could not unpack cache"
    rm ${SANDBOX_HASH}.tar.xz
}

recover_cache
fallback "updating cache"

#!/bin/bash -
#===============================================================================
#
#          FILE: thentos-install.sh
#
#         USAGE: misc/thentos-install.sh [-c <CABAL-OPTS>]
#
#   DESCRIPTION:
#       Installs thentos packages and their dependencies into a cabal
#       sandbox. Use it only from the thentos repo top-level dir.
#
#       CREATED: 14.07.2015 18:02
#===============================================================================

set -o nounset
set -o errexit


declare -a SOURCES
SUBMODULE_SOURCES=( servant/servant
                    servant/servant-server
                    servant/servant-client
                    servant/servant-docs
                    servant/servant-blaze
                    servant/servant-session
                    pronk
                  )
SOURCES=( thentos-core thentos-tests thentos-adhocracy )
ALL_SOURCES=( "${SUBMODULE_SOURCES[@]}" "${SOURCES[@]}" )
SOURCES_STR=$( IFS=$' ', echo ${SOURCES[*]} )

DIR=`pwd`
SANDBOX="$DIR/.cabal-sandbox"
CABAL_ARGS=""

usage () {
    echo "thentos-install.sh [-c <CABAL-OPTS>]"
    echo "  Installs thentos packages and their dependencies into a cabal"
    echo "  sandbox. Use it only from the thentos repo top-level dir."
    exit 1
}

check_dir () {
    if [ "$(basename $DIR)" != "thentos" ] ; then
        echo "Wrong directory"
        usage
    fi
}

while getopts :c: opt; do
    case $opt in
        c) CABAL_ARGS="$OPTARG"
           ;;
        *) echo "Invalid option: -$OPTARG" >&2
           usage
           ;;
    esac
done

check_dir

git submodule update --init
cabal sandbox init

for s in ${SUBMODULE_SOURCES[@]}; do
    cd "submodules/$s"
    cabal sandbox init --sandbox="$SANDBOX"
    cabal sandbox add-source .
    cd $DIR
done

for s in ${SOURCES[@]}; do
    cd "$s"
    cabal sandbox init --sandbox="$SANDBOX"
    cabal sandbox add-source .
    cd $DIR
done

cabal install --dependencies-only -j2 --ghc-options="+RTS -M2G -RTS -w" \
      --enable-tests --enable-bench --max-backjumps -1 --reorder-goals \
      -fwith-thentos-executable $CABAL_ARGS $SOURCES_STR
cabal install \
      --enable-tests --enable-bench --max-backjumps -1 --reorder-goals \
      -fwith-thentos-executable $CABAL_ARGS $SOURCES_STR

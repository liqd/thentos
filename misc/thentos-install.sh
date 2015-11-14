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
NO_PURESCRIPT=""
CABAL_VERBOSITY=""

usage () {
    echo "thentos-install.sh [-c <CABAL-OPTS>] [-p]"
    echo "  Installs thentos packages and their dependencies into a cabal"
    echo "  sandbox. Use it only from the thentos repo top-level dir."
    echo "  '-p' means 'do not build purescript'."
    exit 1
}

check_dir () {
    if [ "$(basename $DIR)" != "thentos" ] ; then
        echo "Wrong directory"
        usage
    fi
}

while getopts :c:p opt; do
    case $opt in
        c) CABAL_ARGS="$OPTARG"
           ;;
        p) NO_PURESCRIPT=1
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

if [ "$NO_PURESCRIPT" == "" ]; then
    echo -e "\n\nbuilding thentos-purescript...\n" >&2
    ./thentos-purescript/build.sh clean
    ./thentos-purescript/build.sh dep
    ./thentos-purescript/build.sh it
fi

echo -e "\n\nbuilding dependencies...\n" >&2
cabal install $CABAL_VERBOSITY --dependencies-only -j2 --ghc-options="+RTS -M2G -RTS -w" \
      --enable-tests --enable-bench --max-backjumps -1 --reorder-goals \
      -fwith-thentos-executable $CABAL_ARGS $SOURCES_STR

echo -e "\n\nbuilding thentos-* packages...\n" >&2
cabal install $CABAL_VERBOSITY -j2 --ghc-options="+RTS -M2G -RTS -w" \
      --enable-tests --enable-bench --max-backjumps -1 --reorder-goals \
      -fwith-thentos-executable $CABAL_ARGS $SOURCES_STR

echo "all done!" >&2

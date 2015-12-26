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
                    servant/servant-js
                    servant/servant-foreign
                    pronk
                  )
SOURCES=( servant-session thentos-core thentos-tests thentos-adhocracy )
ALL_SOURCES=( "${SUBMODULE_SOURCES[@]}" "${SOURCES[@]}" )
SOURCES_STR=$( IFS=$' ', echo ${SOURCES[*]} )

DIR=`pwd`
SANDBOX="$DIR/.cabal-sandbox"
CABAL_ARGS=""
NO_PURESCRIPT=""
DEPS_ONLY=""
THOROUGH=""
CABAL_VERBOSITY=""

usage () {
    echo "thentos-install.sh [-c <CABAL-OPTS>] [-p] [-d] [-t]"
    echo "  Installs thentos packages and their dependencies into a cabal"
    echo "  sandbox. Use it only from the thentos repo top-level dir."
    echo "  '-p' means 'do not build purescript'."
    echo "  '-d' means 'dependencies only'.  cancels out '-t'."
    echo "  '-t' means 'thorough' (compiles with -Werror, runs hlint and test suite)."
    exit 1
}

check_dir () {
    if [ "$(basename $DIR)" != "thentos" ] ; then
        echo "Wrong directory"
        usage
    fi
}

while getopts c:pdt opt; do
    case $opt in
        c) CABAL_ARGS="$OPTARG"
           ;;
        p) NO_PURESCRIPT=1
           ;;
        d) DEPS_ONLY=1
           ;;
        t) THOROUGH=1
           ;;
        *) echo "Invalid option: -$OPTARG" >&2
           usage
           ;;
    esac
done

echo "running $0 with -c=\"$CABAL_ARGS\" -p=$NO_PURESCRIPT -d=$DEPS_ONLY =t=$THOROUGH" >&2

check_dir

git submodule sync
git submodule update --init
test -e cabal.sandbox.config || cabal sandbox init

for s in ${SUBMODULE_SOURCES[@]}; do
    cd "submodules/$s"
    test -e cabal.sandbox.config || ( cabal sandbox init --sandbox="$SANDBOX"; cabal sandbox add-source . )
    cd $DIR
done

for s in ${SOURCES[@]}; do
    cd "$s"
    test -e cabal.sandbox.config || ( cabal sandbox init --sandbox="$SANDBOX"; cabal sandbox add-source . )
    cd $DIR
done

if [ "$NO_PURESCRIPT" == "" ]; then
    echo -e "\n\nbuilding thentos-purescript...\n" >&2
    ./thentos-purescript/build.sh clean
    ./thentos-purescript/build.sh dep
    ./thentos-purescript/build.sh it
fi

function build() {
    cabal install $CABAL_VERBOSITY $1 --ghc-options="+RTS -M2G -RTS -w" \
        --enable-tests --enable-bench --max-backjumps -1 --reorder-goals \
        -fwith-thentos-executable $CABAL_ARGS $SOURCES_STR
}

echo -e "\n\nbuilding dependencies...\n" >&2
build "--dependencies-only"

if [ "$DEPS_ONLY" == "" ]; then
    echo -e "\n\nbuilding thentos-* packages...\n" >&2
    build ""
    if [ "$THOROUGH" == "1" ]; then
        make hlint
        build "--ghc-options=-Werror"
        for s in ${SOURCES[@]}; do
            cd $s
            grep -q ^test-suite $s.cabal && cabal test
            cd ..
        done
    fi
fi

echo "all done!" >&2

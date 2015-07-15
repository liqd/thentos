#!/bin/bash -
#===============================================================================
#
#          FILE: thentos-install.sh
#
#         USAGE: misc/thentos-install.sh
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
SERVANT_SOURCES=( servant servant-server servant-client servant-docs servant-blaze )
SOURCES=( thentos-core thentos-tests thentos-adhocracy )
ALL_SOURCES=( "${SERVANT_SOURCES[@]}" "${SOURCES[@]}" )
SOURCES_STR=$( IFS=$' ', echo ${SOURCES[*]} )

DIR=`pwd`
SANDBOX="$DIR/.cabal-sandbox"

usage () {
    echo "thentos-install.sh"
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

[ $# == 0 ] || usage
check_dir

git submodule update --init
cabal sandbox init

for s in ${SERVANT_SOURCES[@]}; do
    cd "submodules/servant/$s"
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

cabal install --enable-tests --max-backjumps -1 --reorder-goals -fwith-thentos-executable $SOURCES_STR

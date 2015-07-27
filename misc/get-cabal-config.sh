#!/bin/bash -
#===============================================================================
#
#          FILE: get-cabal-config.sh
#
#         USAGE: ./get-cabal-config.sh [STACK.YML] [RESULT-FILE]
#
#   DESCRIPTION: Generate a cabal config file corresponding to the specified
#                stack.yml file.
#
#  REQUIREMENTS: curl
#       CREATED: 27.07.2015 15:57
#===============================================================================

set -o nounset
set -o errexit

STACK_FILE="$1"
RESULT_FILE="$2"
RESOLVER=$(cat "$STACK_FILE" | grep resolver | cut -d':' -f 2 | xargs)
LINK="https://www.stackage.org/$RESOLVER/cabal.config"

WITHIN_DEPS=0
curl $LINK > $RESULT_FILE
LAST=$(tail -1 $RESULT_FILE)

sed -i '$ d' $RESULT_FILE


parse_dep () {
    local depline=$1
    local version="$(echo $depline | grep -o '[0-9.]*$')"
    local x="$(echo $depline | grep -o '^[^0-9]*' | grep -o '[^ ]*$')"
    local package="${x::-1}"
    local res="$package ==$version,"
    # remove other instances
    sed -i '/^ *'"$package"'/d' $RESULT_FILE
    echo "        $res" >> $RESULT_FILE
}

remove_servant () {
    sed -i '/^ *servant/d' $RESULT_FILE
}

cat "$STACK_FILE" | while read line ; do
    case $line in
        extra-deps*) WITHIN_DEPS=1;;
        -*) if [ "$WITHIN_DEPS" -eq 1 ] ; then parse_dep "$line" ; fi ;;
        *) WITHIN_DEPS=0;
    esac
done

remove_servant

echo "$LAST" >> $RESULT_FILE

#!/bin/bash

set -o errexit
cd "$( dirname "${BASH_SOURCE[0]}" )"

export PATH=`pwd`/node_modules/.bin/:$PATH

# a bug in pulp-4.4.1 makes pulp not exit with non-0 status code to
# the system in case of a non-zero exit from psc.  this hides psc
# compiler errors effectively from travis.
#
# WORKAROUND: turn off -O.
# FIX: https://github.com/bodil/pulp/pull/97

THENTOS_PURESCRIPT_OPTS=""
#THENTOS_PURESCRIPT_OPTS="-O"

case "$1" in
    "dep")
        npm install --dev
        time pulp dep install
        ;;
    "it")
        time pulp browserify $THENTOS_PURESCRIPT_OPTS --to ./static/thentos.js
        ;;
    "watch")
        pulp --watch browserify --to ./static/thentos.js
        ;;
    "clean")
        rm -rf ./output/
        if [ -d ./bower_components/ ]; then
            cd ./bower_components/
            pulp dep uninstall * 2>/dev/null || true
            cd ../
        fi
        ;;
    "distclean")
        $0 clean
        rm -rf .pulp-cache ./bower_components ./node_modules
        ;;
    "generate")
        if [ "$2" != "" ]; then
            URL="$2"
        else
            URL="http://localhost:7001/docs"
        fi
        curl $URL/purs/Servant.Simple > src/Servant/Simple.purs
        curl $URL/purs/Util.js > src/Util.js
        curl $URL/purs/Util.purs > src/Util.purs
        ;;
    *)
        echo "usage: $0 [dep|it|watch|clean|generate]" >&2
        exit 1
        ;;
esac

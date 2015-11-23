#!/bin/bash

set -o errexit
cd "$( dirname "${BASH_SOURCE[0]}" )"

case "$1" in
    "dep")
        npm install virtual-dom
        time pulp dep install
        ;;
    "it")
        time pulp browserify -O --to ./static/thentos.js
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

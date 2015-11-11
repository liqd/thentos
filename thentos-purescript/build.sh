#!/bin/bash

set -o errexit
cd "$( dirname "${BASH_SOURCE[0]}" )"

case "$1" in
    "dep")
        npm install virtual-dom
        pulp dep install
        ;;
    "it")
        pulp browserify -O --to ./static/thentos.js
        ;;
    "watch")
        pulp --watch browserify --to ./static/thentos.js
        ;;
    "clean")
        rm -rf ./output/
        cd ./bower_components/
        pulp dep uninstall * 2>/dev/null || true
        cd ../
        ;;
    *)
        echo "usage: $0 [dep|it|watch|clean]" >&2
        exit 1
        ;;
esac

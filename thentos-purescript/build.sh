#!/bin/bash

set -o errexit
cd "$( dirname "${BASH_SOURCE[0]}" )"

case "$1" in
    "")
        npm install virtual-dom
        pulp dep install
        pulp browserify -O --to ./static/thentos.js
        ;;
    "-i")
        pulp --watch browserify --to ./static/thentos.js
        ;;
    *)
        echo "usage: $0 [-i]" >&2
        exit 1
        ;;
esac

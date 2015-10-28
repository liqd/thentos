#!/bin/bash

set -o errexit
cd "$( dirname "${BASH_SOURCE[0]}" )"

case "$1" in
    "")
        mkdir -p ./static/js/
        pulp dep install
        pulp build -O --to ./static/js/thentos.js
        ;;
    "-i")
        pulp --watch build --to ./static/js/thentos.js
        ;;
    *)
        echo "usage: $0 [-i]" >&2
        exit 1
        ;;
esac

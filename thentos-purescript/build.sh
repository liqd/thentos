#!/bin/bash

set -o errexit
cd "$( dirname "${BASH_SOURCE[0]}" )"

case "$1" in
    "")
        pulp dep install
        pulp build -O --to ./static/thentos.js
        ;;
    "-i")
        pulp --watch build --to ./static/thentos.js
        ;;
    *)
        echo "usage: $0 [-i]" >&2
        exit 1
        ;;
esac

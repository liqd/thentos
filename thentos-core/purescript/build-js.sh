#!/bin/bash

set -o errexit
cd "$( dirname "${BASH_SOURCE[0]}" )"

case "$1" in
    "")
        mkdir -p ../js
        pulp dep install
        pulp build -O --to ../js/thentos.js
        ;;
    "-i")
        pulp --watch build --to ../js/thentos.js
        ;;
    *)
        echo "usage: $0 [-i]" >&2
        exit 1
        ;;
esac

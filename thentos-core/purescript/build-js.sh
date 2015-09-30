#!/bin/bash

set -o errexit
cd "$( dirname "${BASH_SOURCE[0]}" )"

mkdir -p ../js
pulp dep install
pulp build -O --to ../js/thentos.js

#!/bin/sh

mkdir -p js                         || exit $?
cd purescript                       || exit $?
pulp dep install                    || exit $?
pulp build -O --to ../js/thentos.js

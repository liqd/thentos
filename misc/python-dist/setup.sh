#!/bin/bash

PYTHON_PACKAGE=thentos_adhocracy

set -o errexit
cd "$( dirname "${BASH_SOURCE[0]}" )"

mkdir -p $PYTHON_PACKAGE/{bin,etc}

cp ../../.cabal-sandbox/bin/thentos-adhocracy $PYTHON_PACKAGE/bin/ \
    || ( echo "no thentos-adhocracy executable" && exit 1 )
cp ../../thentos-adhocracy/devel.config $PYTHON_PACKAGE/etc/thentos.yaml \
    || ( echo "no thentos-adhocracy config file" && exit 1 )

rm -rf dist

for cmd in clean sdist; do  # bdist bdist_wheel
    python3 setup.py $cmd;
done

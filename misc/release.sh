#!/bin/bash

# release.sh
#
# Build a Thentos binary release tarball.
# Must be run from the base of the thentos project, 
#
# Command line arguments:
#
# [git ref name to release]
#


set -e

if [ -z "$1" ]; then
    echo "Please provide a git reference name for release"
    exit 1
fi

if ! git show-ref $1 >> /dev/null; then
    echo "\"$1\" is not a valid git reference"
    exit 1
fi

if [ ! -f cabal.sandbox.config ]; then
    echo "cabal.sandbox.config not found"
    exit 1
fi

relname=thentos-$1.bin
tmpdir=`mktemp -d`
gitdir=`pwd`
cabal_sandbox=`cat cabal.sandbox.config | grep '^ *prefix' | awk -F ' ' '{print $2}'`

pushd $tmpdir
git clone --reference $gitdir --branch $1 $gitdir
cd thentos
cabal sandbox init --sandbox=$cabal_sandbox
CABAL_PACKAGE_SOURCE_ROOT_THENTOS_CORE=. \
    misc/thentos-install.hs -p 
cd ..
mkdir $relname
cd $relname
cp $cabal_sandbox/bin/thentos-adhocracy . # TODO: copy correct binary(s)
mkdir -p thentos-core/schema
cp -r ../thentos/thentos-core/schema/* thentos-core/schema
#TODO: Copy config
cd ..
tar cf $relname.tar.gz *
popd
cp $tmpdir/$relname.tar.gz .
rm -rf $tmpdir
md5sum $relname.tar.gz
echo "All done."

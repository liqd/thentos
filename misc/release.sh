#!/bin/bash

# release.sh
#
# Build a Thentos binary release tarball.
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

cd `dirname $0`/..

if ! git show-ref $1 >> /dev/null; then
    echo "\"$1\" is not a valid git reference"
    exit 1
fi

if [ ! -f cabal.sandbox.config ]; then
    echo "cabal.sandbox.config not found"
    exit 1
fi

relname=thentos-$1
tarballtar=$relname.bin.tar
tarball=$tarballtar.gz
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
tar cf $tarballtar *
gzip $tarballtar
sha1hash=`sha1sum $tarball | awk -F ' ' '{print $1}'`
md5hash=`md5sum $tarball | awk -F ' ' '{print $1}'`
echo -n $md5hash > $tarball.md5
echo -n $sha1hash > $tarball.sha1
popd
cp $tmpdir/$tarball* .
rm -rf $tmpdir
echo "All done."
echo "MD5:  $md5hash"
echo "SHA1: $sha1hash"

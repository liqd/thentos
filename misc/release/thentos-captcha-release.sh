#!/bin/bash

# release.sh
#
# Build a Thentos binary release targz.
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

cd `dirname $0`/../..

if ! git show-ref $1 >> /dev/null; then
    echo "\"$1\" is not a valid git reference"
    exit 1
fi

if [ ! -f cabal.sandbox.config ]; then
    echo "cabal.sandbox.config not found"
    exit 1
fi

relname=thentos-captcha-$1
tar=$relname.bin.tar
targz=$tar.gz
tmpdir=`mktemp -d`
gitdir=`pwd`
cabal_sandbox=`cat cabal.sandbox.config | grep '^ *prefix' | awk -F ' ' '{print $2}'`

pushd $tmpdir
git clone --reference $gitdir --branch $1 $gitdir
cd thentos
cabal sandbox init --sandbox=$cabal_sandbox
./misc/thentos-install.hs -p
cd ..
mkdir $relname
cd $relname
cp $cabal_sandbox/bin/thentos-captcha .
mkdir -p thentos-core/schema
cp -r ../thentos/thentos-core/schema/* thentos-core/schema
cp ../thentos/misc/release/thentos-captcha-README.md README.md
cp ../thentos/thentos-core/example.config thentos-captcha.config
cd ..
tar cf $tar *
gzip $tar
sha1hash=`sha1sum $targz | awk -F ' ' '{print $1}'`
md5hash=`md5sum $targz | awk -F ' ' '{print $1}'`
echo -n $md5hash > $targz.md5
echo -n $sha1hash > $targz.sha1
popd
mkdir -p "output/releases"
cp $tmpdir/$targz* output/releases/
rm -rf $tmpdir
echo "Created release tarball in output/releases/$targz"
echo "MD5:  $md5hash"
echo "SHA1: $sha1hash"

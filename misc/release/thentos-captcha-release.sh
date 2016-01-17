#!/bin/bash

# release.sh
#
# Build a Thentos binary release.
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
tarxz=$relname.`uname -m`.tar.xz
tmpdir=`mktemp -d`
gitdir=`pwd`
cabal_sandbox=`cat cabal.sandbox.config | grep '^ *prefix' | awk -F ' ' '{print $2}'`

echo -e "\n\n\n"
echo "git ref: $1"
echo "release file: $tarxz"
echo "build path: `pwd`"
echo -e "\n\n\n"

pushd $tmpdir

# clone and build
git clone --reference $gitdir --branch $1 $gitdir
cd thentos
cabal sandbox init --sandbox=$cabal_sandbox
./misc/thentos-install.hs -p
cd ..

# collect files
mkdir $relname
cd $relname
cp $cabal_sandbox/bin/thentos-captcha .
cp -r ../thentos/thentos-core/schema .
cp -r ../thentos/thentos-core/resources .
cp ../thentos/misc/release/thentos-captcha-README.md README.md
cp ../thentos/thentos-core/devel.config thentos-captcha.config
cd ..

# build package, cleanup, report
tar cJf $tarxz $relname
popd
mkdir -p "output/releases"
mv $tmpdir/$tarxz* output/releases/
rm -rf $tmpdir
echo "Created release tarball in output/releases/$tarxz"
cd ./output/releases
for hash in sha256 sha1 md5; do
    echo -n "${hash}: "
    ${hash}sum $tarxz | awk -F ' ' '{print $1}' | tee $tarxz.$hash
done
cd -

#!/bin/bash

# terminate this script after $TIMEOUT minutes so that travis updates
# the cache before failing.
TIMEOUT=$[2*60-5]
THIS_PID=$$
( sleep $[$TIMEOUT*60] ; kill $THIS_PID ) &

# We install hlint because we don't want to invalidate i's cache together
# with the thentos cache
git clone --depth 1 https://github.com/liqd/travis-cache
export PATH=$PWD/travis-cache:$PATH
mkdir -p ./hlint/.cabal-sandbox/share/x86_64-linux-ghc-7.8.4
cp -r travis-cache/data ./hlint/.cabal-sandbox/share/x86_64-linux-ghc-7.8.4/hlint-1.9.21/

# The container reports 16 cores (when it only has 1.5), and a higher
# number may result in slower builds. So we comment out the relevant line
# the config file
sed -i 's/^jobs:/-- jobs:/' ${HOME}/.cabal/config

cabal update

echo -e "\n\n>>> purescript\n\n"
pulp --version || npm install -g pulp@4.4.1
psc  --version || npm install -g purescript@0.7.6

echo -e "\n\n>>> ghc (deps only)\n\n"
./misc/thentos-install.hs -c "--force-reinstalls" -t

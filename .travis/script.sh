#!/bin/bash

# terminate this script after $TIMEOUT minutes so that travis updates
# the cache before failing.
TIMEOUT=$[2*60-5]
THIS_PID=$$
( sleep $[$TIMEOUT*60] ; kill $THIS_PID ) &

# The container reports 16 cores (when it only has 1.5), and a higher
# number may result in slower builds. So we comment out the relevant line
# the config file
sed -i 's/^jobs:/-- jobs:/' ${HOME}/.cabal/config

cabal update
./misc/thentos-install.hs -c "--force-reinstalls" -t -p

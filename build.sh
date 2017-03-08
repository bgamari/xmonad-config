#!/bin/bash -e

cabal new-build --disable-library-profiling xmonad-ben

rm -f xmonad-x86_64-linux
cp bin/xmonad-ben bin/xmonad
cp bin/xmonad xmonad-x86_64-linux
cp bin/xmonad $HOME/.cabal/bin
echo "installed."

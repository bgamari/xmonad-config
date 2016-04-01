#!/bin/bash

sudo apt-get install compton gmrun libxinerama-dev feh

git clone git://github.com/bgamari/taffybar
cabal install dbus taffybar/


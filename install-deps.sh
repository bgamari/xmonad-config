#!/bin/bash

sudo apt-get install compton gmrun

git clone git://github.com/bgamari/taffybar
cabal install dbus taffybar/


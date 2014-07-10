#!/bin/bash

sudo apt-get install xcompmgr

git clone git://github.com/bgamari/taffybar
cabal install dbus taffybar/


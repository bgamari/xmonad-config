#!/bin/bash

sudo apt-get install xcompmgr gmrun

git clone git://github.com/bgamari/taffybar
cabal install dbus taffybar/


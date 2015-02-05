#!/bin/bash

export UBUNTU_MENUPROXY=
PATH=$HOME/.cabal/bin:$PATH
# Prefix necessary for proper GHC
source $HOME/.env/prefix/prefix.sh

$HOME/.env/bin/set-synaptics
start-pulseaudio-x11
xfsettingsd &
nm-applet &
blueman-applet &
xmonad


#!/bin/bash

export UBUNTU_MENUPROXY=
PATH=$HOME/.cabal/bin:$PATH

$HOME/.env/bin/set-synaptics
start-pulseaudio-x11
xfsettingsd &
nm-applet &
blueman-applet &
xmonad


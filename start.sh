#!/bin/bash

#gnome-power-manager &

export UBUNTU_MENUPROXY=
PATH=$HOME/.cabal/bin:$PATH

$HOME/.env/bin/set-synaptics
start-pulseaudio-x11
#gnome-settings-daemon &
nm-applet &
blueman-applet &
xmonad


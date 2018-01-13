#!/bin/bash

# 17 June 2016: Use Xinput2 for drag scrolling
export MOZ_USE_XINPUT2=1

export WINDOW_MANAGER=xmonad
export UBUNTU_MENUPROXY=
PATH=$HOME/.cabal/bin:$PATH
# Prefix necessary for proper GHC
source $HOME/.env/prefix/prefix.sh

$HOME/.env/bin/set-synaptics
start-pulseaudio-x11
gnome-settings-daemon &
nm-applet --sm-disable &
system-config-printer-applet &
blueman-applet &
xdg-user-dirs-gtk-update
$HOME/.xmonad/set-bg.sh
exec xmonad-ben

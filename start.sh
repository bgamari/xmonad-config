#!/bin/bash

export WINDOW_MANAGER=xmonad
export UBUNTU_MENUPROXY=
PATH=$HOME/.cabal/bin:$PATH
# Prefix necessary for proper GHC
source $HOME/.env/prefix/prefix.sh

$HOME/.env/bin/set-synaptics
start-pulseaudio-x11
gnome-settings-daemon &
nm-applet --sm-disable &
eval $(gpg-agent --daemon)
system-config-printer-applet &
blueman-applet &
xdg-user-dirs-gtk-update
$HOME/.xmonad/set-bg.sh
exec xmonad


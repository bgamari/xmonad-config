#!/bin/bash

ln -s .xmonad/xsession ~/.xsession

mkdir ~/.config
ln -s $HOME/.xmonad/taffybar $HOME/.config/taffybar

session=/usr/share/xsession/custom.desktop
if [ ! -f $session ]; then
echo "Installing display manager session"
sudo cat <<EOF >$session
[Desktop Entry]
Name=Xsession
Exec=/etc/X11/Xsession
EOF
fi


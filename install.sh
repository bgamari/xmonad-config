#!/bin/bash

ln -s .xmonad/xsession ~/.xsession

if [ ! -e $HOME/.config/taffybar ]; then
        ln -s $HOME/.xmonad/taffybar $HOME/.config/taffybar
fi

xsessions=/usr/share/xsessions/custom.desktop
if [ ! -f $xsessions/custom.desktop ]; then
        echo "Installing display manager session"
        cat <<EOF >custom.desktop
[Desktop Entry]
Name=Xsession
Exec=/etc/X11/Xsession
EOF
        sudo mv custom.desktop $xsessions
fi

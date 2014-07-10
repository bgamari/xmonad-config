#!/bin/bash

ln -s .xmonad/xsession ~/.xsession

mkdir ~/.config
ln -s $HOME/.xmonad/taffybar $HOME/.config/taffybar

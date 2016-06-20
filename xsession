#!/bin/bash

# 17 June 2016: Use Xinput2 for drag scrolling
export MOZ_USE_XINPUT2=1

exec dbus-launch --sh-syntax --exit-with-session $HOME/.xmonad/start.sh

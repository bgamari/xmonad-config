{-# LANGUAGE OverloadedStrings #-}

module Brightness (Direction(..), modifyBrightness) where

import Control.Error
import Control.Monad (void)
import DBus
import DBus.Client

data Direction = Down | Up deriving (Show)

modifyBrightness :: Client -> Direction -> EitherT String IO ()
modifyBrightness client dir =
    let m = case dir of Up   -> "StepUp"
                        Down -> "StepDown"
    in void $ fmapLT show $ EitherT $ call client
       $ (methodCall "/org/gnome/SettingsDaemon/Power" "org.gnome.SettingsDaemon.Power.Screen" m)

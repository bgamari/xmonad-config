{-# LANGUAGE OverloadedStrings #-}

module Brightness (Direction(..), modifyBrightness) where

import Control.Error
import Control.Monad (void)
import System.Process

data Direction = Down | Up deriving (Show)

modifyBrightness :: Direction -> ExceptT String IO ()
modifyBrightness dir =
    let m = case dir of Up   -> "+5"
                        Down -> "-5"
    in withExcept show $ tryIO $ callProcess "set-brightness" [m]

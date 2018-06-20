-- -*- mode:haskell -*-
module Main where

import System.Taffybar
import System.Taffybar.Hooks
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.FreedesktopNotifications
import System.Taffybar.Widget.Generic.PollingLabel
import System.Taffybar.Widget.Util
import System.Taffybar.Widget.Workspaces
import System.Log.Logger

main = do
  let myWorkspacesConfig =
        defaultWorkspacesConfig
        { minIcons = 0
        , maxIcons = Just 0
        , widgetGap = 0
        , showWorkspaceFn = hideEmpty
        , getWindowIconPixbuf = \_ _ -> return Nothing
        , updateEvents =
          [ "_NET_CURRENT_DESKTOP"
          , "_NET_NUMBER_OF_DESKTOPS"
          , "_NET_DESKTOP_NAMES"
          , "_NET_NUMBER_OF_DESKTOPS"
          ]
        , updateRateLimitMicroseconds = 200000
        }
      workspaces = workspacesNew myWorkspacesConfig
      clock = textClockNew Nothing "%a %b %_d %r" 1
      layout = layoutNew defaultLayoutConfig
      windows = windowsNew defaultWindowsConfig
          -- See https://github.com/taffybar/gtk-sni-tray#statusnotifierwatcher
          -- for a better way to set up the sni tray
      tray = sniTrayNew
      myConfig = defaultSimpleTaffyConfig
        { startWidgets =
            workspaces :
            map (>>= buildContentsBox) [ layout, windows ]
        , endWidgets = map (>>= buildContentsBox)
          [ batteryIconNew
          , clock
          , tray
          , notifyAreaNew defaultNotificationConfig
          , mpris2New
          ]
        , barPosition = Top
        , barPadding = 0
        , barHeight = 25
        , widgetSpacing = 0
        }
  --dyreTaffybar $ withBatteryRefresh $ withLogServer $ withToggleServer $
  --             toTaffyConfig myConfig

  logger <- getLogger "System.Taffybar"
  saveGlobalLogger $ setLevel INFO logger

  startTaffybar $ withBatteryRefresh $ withLogServer $ withToggleServer $ toTaffyConfig myConfig

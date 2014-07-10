{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mconcat)
import Control.Monad.State
import Data.Traversable
import System.Process
import qualified Data.Text as T
import Text.Printf

import System.Taffybar
import System.Taffybar.Systray
import System.Taffybar.Pager
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Weather
import System.Taffybar.MPRIS
import System.Taffybar.Battery

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.Widgets.PollingLabel

import System.Information.Memory
import System.Information.CPU

import Graphics.UI.Gtk (Markup, widgetShow)

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

mailCallback = do
  n <- readProcess "notmuch" ["count", "tag:inbox", "and", "tag:unseen"] ""
  if n /= "0"
    then return $ "✉ " ++ colorize "blue" "" n
    else return ""

main = do
  let memCfg = defaultGraphConfig { graphDataColors = [(1, 0, 0, 1)]
                                  , graphLabel = Nothing
                                  }
      cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
                                                      , (1, 0, 1, 0.5)
                                                      ]
                                  , graphLabel = Nothing
                                  }
  let clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 60
      log = taffyPagerNew pagerConfig
      note = notifyAreaNew defaultNotificationConfig { notificationFormatter = formatter }
      wea = weatherNew (defaultWeatherConfig "KCEF") 10
      mpris = mprisNew
      battery = batteryBarNew defaultBatteryConfig 120
      mem = pollingGraphNew memCfg 10 memCallback
      cpu = pollingGraphNew cpuCfg 10 cpuCallback
      tray = systrayNew
  mail <- pollingLabelNew "mail" 30 mailCallback
  widgetShow mail
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ log, note ]
                                        , endWidgets = [ wea, clock, mpris, battery, return mail, tray ]
                                        }

formatter :: Notification -> String
formatter note = msg
  where
    msg = case T.null (noteBody note) of
      True  -> T.unpack $ noteSummary note
      False -> T.unpack $ mconcat [ "<span fgcolor='blue'>Note:</span>"
                                  , noteSummary note, ": "
                                  , head $ T.lines $ noteBody note ]

pagerConfig =
  defaultPagerConfig
  { activeWindow = colorize "#007009" "" . escape . shorten 40
  , markupWorkspaces = myMarkupWorkspaces
  }

weightize :: String -- ^ Weight.
          -> String -- ^ Contents.
          -> Markup
weightize w = printf "<span%s>%s</span>" (attr "weight" w)
  where attr :: String -> String -> String
        attr name value
          | null value = ""
          | otherwise  = printf " %s=\"%s\"" name value

myMarkupWorkspaces :: Traversable f => f WorkspaceInfo -> f Markup
myMarkupWorkspaces wss = evalState (traverse f wss) ("", cycle colors)
  where
    colors = [ "#843c3c"
             , "#60843c"
             , "#3c8384"
             , "#3c4d84"
             , "#613c84"
             , "#843c3c"
             ]

    f :: WorkspaceInfo -> State (String, [String]) Markup
    f ws@(WSInfo {wsiName=name}) = do
      let group = takeWhile (/= '-') name
      (lastGroup, grpColor:rest) <- get
      if group /= lastGroup
        then put (group, rest) >> f ws
        else return $ color grpColor ws

    color grpColor ws@(WSInfo {wsiName=name, wsiVisibility=vis})
      | wsiUrgent ws   = colorize grpColor "#dcb2b2" $ escape name
      | Active <- vis  = colorize grpColor "#b2dcc8" $ weightize "bold" $ escape name
      | Visible <- vis = weightize "bold" $ escape name
      | wsiEmpty ws    = colorize "#868686" "" $ escape name
      | otherwise      = colorize grpColor "" $ escape name

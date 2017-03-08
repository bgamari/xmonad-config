{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE CPP #-}

#define TAFFYBAR 1

import XMonad
import Data.Monoid
import Data.List (isInfixOf)
import Control.Monad (void, filterM)
import Control.Monad.IO.Class
import Control.Error
import System.Exit

import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Util.Run
import XMonad.Prompt

import XMonad.Layout.LimitWindows
import XMonad.Layout.WorkspaceDir

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook

import XMonad.Actions.DynamicWorkspaces hiding (selectWorkspace)
import WorkspacePrompt                         (selectWorkspace)
import XMonad.Actions.CopyWindow (copy)
import XMonad.Actions.CycleRecentWS (cycleRecentWS)
import XMonad.Actions.CycleWindows (cycleRecentWindows)
import XMonad.Actions.CycleWS

import XMonad.Hooks.EwmhDesktops (ewmh)
#ifdef TAFFYBAR
import System.Taffybar.Hooks.PagerHints (pagerHints)
#endif

import MPRIS2
import TrackPlayers
import qualified PulseAudio as PA
import Brightness
import DBus
import DBus.Client

#ifndef TAFFYBAR
pagerHints = id
#endif

--myTerminal      = "konsole"
myTerminal      = "gnome-terminal"
myBrowser       = "firefox"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth   = 2

myModMask       = mod4Mask

myWorkspaces    = ["1","2","3","4","mail","irc"]

--myNormalBorderColor  = "#dddddd"
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#70a0e0"

myXPConfig :: XPConfig
myXPConfig = def { font = "xft:Bitstream Vera Sans-11"
                 , height = 30
                 }

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm              , xK_F1    ), spawn $ XMonad.terminal conf)

    -- launch browser
    , ((modm              , xK_F2    ), spawn myBrowser)

    , ((modm              , xK_F3    ), spawn "nautilus -w .")
    , ((modm              , xK_F4    ), spawn "emacsclient -c .")
    , ((modm              , xK_F5    ), spawn "emacsclient -c -e \"(notmuch)\"")

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_r     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Switch to urgent window
    , ((modm,               xK_u     ), focusUrgent )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Increase the window limit
    , ((modm              , xK_equal ), increaseLimit)
    -- Decrease the window limit
    , ((modm              , xK_minus ), decreaseLimit)

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Remove workspace
    , ((modm              , xK_BackSpace), removeWorkspace)
    -- Select workspace
    , ((modm              , xK_v        ), selectWorkspace myXPConfig)
    -- Move window to workspace
    , ((modm .|. shiftMask, xK_v        ), withWorkspace myXPConfig (windows . W.shift))
    -- Copy window to workspace
    , ((modm              , xK_c        ), withWorkspace myXPConfig (windows . copy))
    -- Kill other copies
    -- , ((modm              , xK_x        ), killAllOtherCopies)
    -- Rename workspace
    , ((modm              , xK_r        ), renameWorkspace myXPConfig)
    -- Cycle through recent workspaces
    , ((modm              , xK_Tab      ), cycleRecentWS [xK_Super_L] xK_Tab xK_grave)
    -- Cycle through recent windows
    , ((mod1Mask          , xK_Tab      ), cycleRecentWindows [xK_Alt_L] xK_Tab xK_grave)

    -- Next workspace
    , ((modm              , xK_Right    ), moveTo Next HiddenNonEmptyWS)
    -- Previous workspace
    , ((modm              , xK_Left     ), moveTo Prev HiddenNonEmptyWS)
    -- Next workspace
    , ((modm .|. shiftMask, xK_Right    ), shiftTo Next HiddenNonEmptyWS)
    -- Previous workspace
    , ((modm .|. shiftMask, xK_Left     ), shiftTo Prev HiddenNonEmptyWS)

    -- Set workspace directory
    , ((modm              , xK_d        ), changeDir myXPConfig)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "/home/ben/.xmonad/xmonad --restart")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), withNthWorkspace f i)
        | (i, k) <- zip [0..] [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- * NOTE: XMonad.Hooks.EwmhDesktops users must remove the obsolete
-- ewmhDesktopsLayout modifier from layoutHook. It no longer exists.
-- Instead use the 'ewmh' function from that module to modify your
-- defaultConfig as a whole. (See also logHook, handleEventHook, and
-- startupHook ewmh notes.)
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = workspaceDir "/home/ben"
         $ limitWindows 6
         $ tiled ||| Mirror tiled ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ isFullscreen                  --> doFullFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "Pidgin"         --> doShift "chat"
    , className =? "empathy-chat"   --> doShift "chat"
    , className =? "rhythmbox"      --> doShift "music"
    ]


------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add initialization of EWMH support to your custom startup
-- hook by combining it with ewmhDesktopsStartup.
--
myStartupHook = return ()

-- | Run an EitherT action producing output on error
noteExceptT :: MonadIO m => ExceptT String m a -> m (Maybe a)
noteExceptT action = runExceptT action >>= either f (return . Just)
  where
    f err = do liftIO $ putStrLn $ "error: "++err
               return Nothing

setupVolumeKeys :: ExceptT String IO (M.Map (ButtonMask, KeySym) (X ()))
setupVolumeKeys = do
    pulse <- PA.connect
    devices <- PA.getDevices pulse
    matching <- filterM (\dev->not . ("hdmi" `isInfixOf`) <$> PA.getDeviceName pulse dev) devices
    device <- tryHead "No PulseAudio devices" matching
    return $ M.fromList
      [ ( (0, xF86XK_AudioLowerVolume)
        , void $ io $ noteExceptT $ PA.adjustDeviceVolume pulse device (PA.mulVolume 0.9))
      , ( (0, xF86XK_AudioRaiseVolume)
        , void $ io $ noteExceptT $ PA.adjustDeviceVolume pulse device (PA.mulVolume 1.1))
      , ( (0, xF86XK_AudioMute)
        , void $ io $ noteExceptT $ PA.toggleDeviceMute pulse device )
      ]

setupMediaKeys :: ExceptT String IO (M.Map (ButtonMask, KeySym) (X ()))
setupMediaKeys = do
    session <- liftIO connectSession
    playerList <- trackPlayers session
    return $ M.fromList
      [ ( (controlMask, xF86XK_AudioLowerVolume)
        , void $ io $ noteExceptT $ withActivePlayer playerList $ previous session)
      , ( (0, xF86XK_AudioPrev)
        , void $ io $ noteExceptT $ withActivePlayer playerList $ previous session)

      , ( (controlMask, xF86XK_AudioRaiseVolume)
        , void $ io $ noteExceptT $ withActivePlayer playerList $ next session)
      , ( (0, xF86XK_AudioNext)
        , void $ io $ noteExceptT $ withActivePlayer playerList $ next session)

      , ( (controlMask, xF86XK_AudioMute)
        , void $ io $ noteExceptT $ withActivePlayer playerList $ playPause session)
      , ( (controlMask, xF86XK_AudioPlay)
        , void $ io $ noteExceptT $ withActivePlayer playerList $ playPause session)
      ]

setupBrightnessKeys :: ExceptT String IO (M.Map (ButtonMask, KeySym) (X()))
setupBrightnessKeys =
    return $ M.fromList
      [ ( (0, xF86XK_MonBrightnessDown)
        , void $ io $ noteExceptT $ modifyBrightness Down)
      , ( (0, xF86XK_MonBrightnessUp)
        , void $ io $ noteExceptT $ modifyBrightness Up)
      ]

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    spawn "compton --backend glx -b"
    spawn "taffybar"
    spawn "arbtt-capture"
    volumeKeys <- runExceptT setupVolumeKeys >>= either (\err->print ("no volume keys: "++err) >> return M.empty) return
    mediaKeys <- runExceptT setupMediaKeys >>= either (\err->print ("no media keys: "++err) >> return M.empty) return
    brightnessKeys <- runExceptT setupBrightnessKeys >>= either (\err->print ("no brightness keys: "++err) >> return M.empty) return
    dbus <- runMaybeT $ hushT $ tryIO $ connectSession
    let pp = defaultPP

    xmonad
      $ ewmh $ pagerHints
      $ withUrgencyHook NoUrgencyHook
      $ def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = \c->myKeys c <> mediaKeys <> volumeKeys <> brightnessKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = avoidStruts $ myLayout,
        manageHook         = manageDocks <+> myManageHook,
        handleEventHook    = docksEventHook,
        logHook            = return (),
        startupHook        = docksStartupHook >> myStartupHook
    }

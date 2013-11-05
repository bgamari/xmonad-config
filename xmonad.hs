{-# LANGUAGE ParallelListComp #-}              

import XMonad
import Data.Colour
import Data.Word (Word8)
import Data.Colour.SRGB (sRGB, sRGB24show)
import Data.Colour.RGBSpace (uncurryRGB)
import Data.Colour.RGBSpace.HSV (hsv)
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.List (sort, groupBy)
import Data.Function (on)
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
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

import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook

import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow (copy)
import XMonad.Actions.CycleRecentWS (cycleRecentWS)
import XMonad.Actions.CycleWindows (cycleRecentWindows)
import XMonad.Actions.CycleWS

import System.Taffybar.XMonadLog
import MPRIS2
import TrackPlayers
import qualified PulseAudio as PA
import DBus
import DBus.Client

myTerminal      = "gnome-terminal"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth   = 1

myModMask       = mod4Mask

myWorkspaces    = ["1","2","3","4","mail","irc"]

myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#0033ff"

myXPConfig :: XPConfig
myXPConfig = def { font = "xft:Ubuntu Mono-10" }

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm              , xK_F1    ), spawn $ XMonad.terminal conf)

    -- launch browser
    , ((modm              , xK_F2    ), spawn "firefox")

    , ((modm              , xK_F3    ), spawn "nautilus .")
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
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Remove workspace
    , ((modm              , xK_BackSpace), removeWorkspace)
    -- Select workspace
    , ((modm              , xK_v        ), selectWorkspace myXPConfig)
    -- Move window to workspace
    , ((modm .|. shiftMask, xK_v        ), withWorkspace myXPConfig (windows . W.shift))
    -- Copy window to workspace
    , ((modm              , xK_c        ), withWorkspace myXPConfig (windows . copy))
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
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
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
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "Pidgin"         --> doShift "chat"
    , className =? "empathy-chat"   --> doShift "chat"
    , className =? "rhythmbox"      --> doShift "music"
    ]


------------------------------------------------------------------------
-- Event handling

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH event handling to your custom event hooks by
-- combining them with ewmhDesktopsEventHook.
--
myEventHook = mempty

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
noteEitherT :: MonadIO m => EitherT String m a -> m (Maybe a)
noteEitherT action = runEitherT action >>= either f (return . Just)
  where
    f err = do liftIO $ putStrLn $ "error: "++err
               return Nothing

setupVolumeKeys :: EitherT String IO (M.Map (ButtonMask, KeySym) (X ()))
setupVolumeKeys = do
    pulse <- PA.connect
    device <- PA.getDevices pulse >>= tryHead "No PulseAudio devices"
    return $ M.fromList
      [ ( (0, xF86XK_AudioLowerVolume)
        , void $ io $ noteEitherT $ PA.adjustDeviceVolume pulse device (PA.mulVolume 0.9))
      , ( (0, xF86XK_AudioRaiseVolume)
        , void $ io $ noteEitherT $ PA.adjustDeviceVolume pulse device (PA.mulVolume 1.1))
      , ( (0, xF86XK_AudioMute)
        , void $ io $ noteEitherT $ PA.toggleDeviceMute pulse device )
      ]

setupMediaKeys :: EitherT String IO (M.Map (ButtonMask, KeySym) (X ()))
setupMediaKeys = do
    session <- liftIO connectSession
    playerList <- trackPlayers session
    return $ M.fromList
      [ ( (controlMask, xF86XK_AudioLowerVolume)
        , void $ io $ noteEitherT $ withActivePlayer playerList $ previous session)
      , ( (controlMask, xF86XK_AudioRaiseVolume)
        , void $ io $ noteEitherT $ withActivePlayer playerList $ next session)
      , ( (controlMask, xF86XK_AudioMute)
        , void $ io $ noteEitherT $ withActivePlayer playerList $ playPause session)
      ]

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    spawn "/usr/bin/xcompmgr -n"
    spawn "/home/ben/.cabal/bin/taffybar"
    volumeKeys <- runEitherT setupVolumeKeys >>= either (\err->print ("no volume keys: "++err) >> return M.empty) return
    mediaKeys <- runEitherT setupMediaKeys >>= either (\err->print ("no media keys: "++err) >> return M.empty) return
    dbus <- runMaybeT $ hushT $ tryIO $ connectSession

    xmonad
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
        keys               = \c->myKeys c <> mediaKeys <> volumeKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = avoidStruts $ myLayout,
        manageHook         = manageDocks <+> myManageHook,
        handleEventHook    = myEventHook,
        logHook            = case dbus of
                               Just client ->
                                 dbusLogWithPP client $ taffybarPP
                                   { ppUrgent  = \s->taffybarColor "black" "#e6abab" . xmobarStrip
                                   , ppCurrent = \s i->taffybarBold $ taffybarColor (showColour $ workspaceColor s i) "#ffffff" $ xmobarStrip i
                                   , ppHiddenNoWindows = \s i->taffybarColor (showColour $ workspaceColor s i) "#dddddd" $ xmobarStrip i
                                   , ppHidden  = \s i->taffybarColor (showColour $ workspaceColor s i) "" $ xmobarStrip i
                                   , ppSep = replicate 8 ' '
                                   , ppWsSep = "  "
                                   }
                               Nothing    -> return ()
    ,   startupHook        = myStartupHook
    }

showColour = sRGB24show 

workspaceColor :: WindowSet -> WorkspaceId -> Colour Float
workspaceColor s i =
    fromMaybe (sRGB 0 0 0)
    $ listToMaybe
    $ map fst
    $ filter (elem i . snd)
    $ colored
  where
    colors = map (uncurryRGB sRGB)
           [ hsv   0 0.8 0.4
           , hsv  60 0.8 0.4
           , hsv 120 0.8 0.4
           , hsv 180 0.8 0.4
           , hsv 240 0.8 0.4
           , hsv 300 0.8 0.4
           ]
    prefix = takeWhile (/= '-')
    colored :: [(Colour Float, [WorkspaceId])]
    colored = zip (cycle colors)
            $ groupBy ((==) `on` prefix) $ sort $ map W.tag $ W.workspaces s

taffybarBold :: String -> String
taffybarBold = wrap "<span weight=\"bold\">" "</span>"

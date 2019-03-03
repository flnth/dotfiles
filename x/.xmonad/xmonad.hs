{-# LANGuAGE RecordWildCards #-}

import XMonad
import XMonad.Core
import XMonad.ManageHook

import ServerMode

-- personal components from ./lib
-- import XMonadEnv

-- prompt
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.AppendFile (appendFilePrompt)

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers

import XMonad.Util.Run(spawnPipe,runProcessWithInput)
import XMonad.Util.EZConfig
import XMonad.Util.Themes
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce
import XMonad.Util.Paste (sendKey)
import XMonad.Util.WindowProperties

import XMonad.Actions.WindowNavigation                          -- for inter- and intra-monitor window navigation
import XMonad.Actions.OnScreen                 -- Control workspaces on different screens
-- import qualified XMonad.Actions.Plane as Plane
-- see https://gist.github.com/vcunat/2da0b57dcbf5b75bf991 for inspration (!)
-- vim keybindings:

import qualified XMonad.Actions.Workscreen as Workscreen        -- for linked workspaces

-- import qualified XMonad.Actions.LinkWorkspaces as LinkWorkspaces

import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow
import XMonad.Actions.GroupNavigation
import XMonad.Actions.Navigation2D

import XMonad.Actions.CycleWS as CycleWS                        -- for moving windows from one screen to the other

import XMonad.Layout.Combo
import XMonad.Layout.ComboP
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed
-- import XMonad.Layout.TabBarDecoration			-- a tab-bar at the top of the layout
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Decoration
import XMonad.Layout.Maximize

import Data.Monoid
import Data.Map (union)

import System.IO
import System.Environment
import System.Exit

import Data.Char

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

------------------------------------------------------------------------
-- * Theme
colorOrange         = "#FD971F"
colorDarkGray       = "#1B1D1E"
colorPink           = "#F92672"
colorGreen          = "#A6E22E"
colorBlue           = "#66D9EF"
colorYellow         = "#E6DB74"
colorWhite          = "#CCCCC6"

-- colorNormalBorder =
-- colorFocusedBorder =

barFont = "terminus"
barXFont = "inconsolata:size=12"
xftFont = "xft: inconsolata-14"
codeNewRomanFont = "xft: CodeNewRoman Nerd Font:pixelsize=10:antialias=true"

-- Prompt Config {{{
mXPConfig :: XPConfig
mXPConfig =
    defaultXPConfig { font                  = barFont
                    , bgColor               = colorDarkGray
                    , fgColor               = colorGreen
                    , bgHLight              = colorGreen
                    , fgHLight              = colorDarkGray
                    , promptBorderWidth     = 0
                    , height                = 14
                    , historyFilter         = deleteConsecutive
                    }

-- Run or Raise Menu
largeXPConfig :: XPConfig
largeXPConfig = mXPConfig
                { font = xftFont
                , height = 22
                }
-- }}}


-- * Bindings

-- Pass the keypresses mask+sym through to win if it has property prop.
-- Otherwise perform def.
keyPassThroughIf :: X () -> Property -> KeyMask -> KeySym -> Window -> X ()
keyPassThroughIf def prop mask sym win = do
  hasprop <- hasProperty prop win
  if hasprop then sendKey mask sym else def

-- ** Keyboard
noMessageFn :: ScreenId -> [Char] -> [Char] -> [Char] -> X()
noMessageFn _ _ _ _ = return () :: X ()

myKeys :: String -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys dir_system conf@(XConfig { modMask = modm, .. }) = mkKeymap conf
    [
      ("M-C-q", spawn "xmonad --recompile; xmonad --restart")
    , ("M-<F12>"		, io (exitWith ExitSuccess))

    , ("M-S-<Return>"   , spawn terminal)
    , ("M-S-f"          , spawn "firefox")
    , ("M-S-e"          , spawn $ dir_system ++ "/emacsclient_gui")
    , ("M-S-x"          , spawn "dmenu_run")
    , ("M-p"		, spawn "dmenu_run")  -- Q: this executed in a shell?
    , ("M-S-p"		, runOrRaisePrompt largeXPConfig)
    , ("M1-<F4>"	, kill)
    , ("M-d"		, kill)
    , ("M-<Space>"	, sendMessage NextLayout)

    , ("M-<Tab>"	, windows W.focusUp)
    , ("M-C-<Tab>"	, windows W.focusDown)
    , ("M-<Page_Up>"	, windows W.focusUp)
    , ("M-<Page_Down>"	, windows W.focusDown)

    , ("M-e"		, shiftNextScreen)

    , ("M-C-h"		, sendMessage Shrink)
    , ("M-C-l"		, sendMessage Expand)
    , ("M-C-j"		, sendMessage MirrorShrink)
    , ("M-C-k"		, sendMessage MirrorExpand)

    , ("M-t"		, withFocused $ windows . W.sink)

    , ("M-b"		, sendMessage ToggleStruts)

    , ("C-M1-l"		, spawn "xscreensaver-command -lock")

    , ("M-<Backspace>"	, removeWorkspace)

    , ("<Print>"        , spawn "shutter -s -n")
    , ("M-<Print>"      , spawn "shutter")

    , ("M-z"		, withFocused (sendMessage . maximizeRestore))

    , ("<F2>"           , namedScratchpadAction (mScratchpads dir_system) "terminal")
    , ("M-x"            , namedScratchpadAction (mScratchpads dir_system) "subterminal")
    , ("<F7>"           , namedScratchpadAction (mScratchpads dir_system) "erc")
    , ("<F6>"           , namedScratchpadAction (mScratchpads dir_system) "spotify")
    -- , ("<F9>"           , namedScratchpadAction (mScratchpads dir_system) "firefox")
    , ("M-<F11>"          , spawn "emacs_capture_win_title.sh" )
    , ("<F12>"          , namedScratchpadAction (mScratchpads dir_system) "top")
    , ("M-<F6>"         , namedScratchpadAction (mScratchpads dir_system) "elfeed")
    , ("M-v"            , windows copyToAll)

    -- , ("M-<F2>"  	, do
    --       w <- (gets (W.tag . W.workspace . W.current . windowset))
    --       namedScratchpadAction (mScratchpads dir_system) "terminal")

    -- , ("<M-<F2>")       ,
      -- 1) look up on which workspace I am  (...somehow chain all that stuff together...)
      -- 2) call up the scratchpad
      -- something like
      -- 	currentWorkspace >>= findScratchpad >>= namedScratchpadAction (mScratchpads dir_system)
      -- so the output of findScratchpad needs to be the last input to namedScratchpadAction

    -- resizeable tall
    -- , ((mod4Mask,	 xK_n), sendMessage MirrorExpand)
    -- , ((mod4Mask,	 xK_m), sendMessage MirrorShrink)
    , ("<XF86AudioMute>"	, spawn "amixer -q -D pulse set Master toggle")
    , ("<XF86AudioLowerVolume>" , spawn "amixer -q set Master 2- unmute")
    , ("<XF86AudioRaiseVolume>" , spawn "amixer -q set Master 2+ unmute")
    , ("<XF86AudioNext>"	, spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
    , ("<XF86AudioPrev>"	, spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
    , ("<XF86AudioStop>"	, spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Stop")
    , ("<XF86AudioPlay>"	, spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")

    -- , ((mod1Mask              , xK_e), W.shift)  -- move window to the other screen and keep the focus
    -- see https://mail.haskell.org/pipermail/xmonad/2012-August/012891.html
    ]

    `M.union` (M.fromList $

    [
        ((mod1Mask, xK_h)      , withFocused $ keyPassThroughIf (windowGo L False) (Or (ClassName "URxvt") (ClassName "Emacs") ) mod1Mask xK_h )
    , ((mod1Mask, xK_j)        , withFocused $ keyPassThroughIf (windowGo D False) (Or (ClassName "URxvt") (ClassName "Emacs") ) mod1Mask xK_j )
    , ((mod1Mask, xK_k)        , withFocused $ keyPassThroughIf (windowGo U False) (Or (ClassName "URxvt") (ClassName "Emacs") ) mod1Mask xK_k )
    , ((mod1Mask, xK_l)        , withFocused $ keyPassThroughIf (windowGo R False) (Or (ClassName "URxvt") (ClassName "Emacs") ) mod1Mask xK_l )
    ]

    ++

    -- fix mod-{w,e,r} mapping
    -- [((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [1,0,2] -- was [0,1,2]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    -- ++

    -- --	Workscreens  (https://github.com/ioreshnikov/dotxmonad/blob/master/xmonad.hs)
    [ ((mod4Mask, key), Workscreen.viewWorkscreen workscreen)
        | (workscreen, key) <- zip [0..9] $ [xK_q, xK_w, xK_a, xK_s]]

    -- [((m .|. modm, k), f i)
    --  | (i, k) <- zip [0..] $ [xK_q, xK_w, xK_a, xK_s]
    --  , (f, m) <- [(Workscreen.viewWorkscreen, 0), (Workscreen.shiftToWorkscreen, shiftMask)]]

    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    )
    --
    -- Plane https://gist.github.com/vcunat/2da0b57dcbf5b75bf991   (check!)
    --

    -- -- Move focus to the next window
    -- , ((modm,               xK_Tab   ), windows W.focusDown)

    -- testkeys = mkKeymap conf
    -- [
    --   ("M-S-Tab", windows W.focusDown)
    -- ]
    -- Resize viewed windows to the correct size
    -- , ((modm,               xK_n     ), refresh)
    -- -- Increment the number of windows in the master area
    -- , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    -- -- Deincrement the number of windows in the master area
    -- , ((modm              , xK_period), sendMessage (IncMasterN (-1)))



    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    -- ++



    ------------------------------------------------------------------------
-- ** Mouse
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
-- * Layouts

-- --> three names + numbers for the rest
-- myWorkspaces = ["test1", "test2", "test3"] ++ map show [4..9]

-- myWorkspaces = ["blah", "blah2", "blah3"]
myWorkspaces = let myOldWorkspaces = ["a", "b", "c", "d"]
                   in Workscreen.expandWorkspace 2 myOldWorkspaces
myStartupHook = do
    Workscreen.configWorkscreen (Workscreen.fromWorkspace 2 myWorkspaces)
    -- spawnOnce "urxvt"
    -- spawnOnce "thunderbird"
    -- spawnOnce "qtcreator" -- TODO: only start at work, not at home
    -- spawnOnce "firefox"
    -- spawnOnce "spotify"

-- myStartupHook = return ()

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
--
mTheme = defaultTheme { decoWidth = -1
                      -- , fontName = "-*-helvetica-bold-r-*-*-18-*-*-*-*-*-*-*"
                      -- , inactiveColor = "black"
                      -- , activeColor = "black"
                      -- , activeTextColor = "red"
                      , inactiveTextColor = "green" }

tabConfig = defaultTheme {
  activeBorderColor = "#00ff00",
  activeTextColor = "#ff0000",
  activeColor = "#000000",
  inactiveBorderColor = "#aaaaaa",
  inactiveTextColor = "#EEEEEE",
  inactiveColor = "#000000",
  decoWidth = 400,
  fontName = codeNewRomanFont
  }


myLayout = maximize $ avoidStruts $ smartBorders $
   ( tiled ||| Mirror tiled ||| tabbed shrinkText tabConfig ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     -- tiled   = Tall nmaster delta ratio
     tiled   = ResizableTall nmaster delta ratio []
     -- tiledm   = ResizableTall nmaster delta ratio []
     -- halfHalf = combineTwoP(TwoPane (3/100) (1/2)) tabs tabs -- 57/100 optimal?
     -- (className browserClass 'Or' )

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

mScratchpads dir_system = [ NS "terminal" spawnTerm findTerm manageTerm
                          , NS "subterminal" spawnSubTerm findSubTerm manageSubTerm
                          , NS "erc"  spawnErc findErc manageErc
                          , NS "spotify"  spawnSpotify findSpotify manageSpotify
                          , NS "firefox"  spawnFirefox findFirefox manageFirefox
                          , NS "top"      spawnTop findTop manageTop
                          , NS "elfeed"   spawnElfeed findElfeed manageElfeed
                          ]

  where
    spawnTerm = "urxvtcd" ++ " -name scratchterm -e emacsclient -nw --socket-name=/tmp/emacs1000/server"
    -- spawnTerm = dir_system ++ "/emacsclient_gui -e '(set-frame-name \"scratchterm\")'"
    -- findTerm  =  stringProperty "_NET_WM_NAME" =? "scratchterm"
    findTerm  =  resource =? "scratchterm"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        t = 0.1
        w = 0.35
        l = (1-w)/2
    spawnErc = "urxvtcd" ++ " -name scratcherc -e emacsclient -nw --socket-name=/tmp/emacs1000/server"
    -- spawnErc = dir_system ++ "/emacsclient_gui -e '(+chat-new-frame \"scratcherc\")'"
    -- findErc  = resource =? "scratcherc"
    -- findErc  = stringProperty "_NET_WM_NAME" =? "scratcherc"
    findErc  = resource =? "scratcherc"
    manageErc = customFloating $ W.RationalRect l t w h
      where
        h = 0.3
        t = 0.05
        w = 0.3
        l = 0.618 - 0.05

    spawnSubTerm = "urxvtcd" ++ " -fn 'xft:CodeNewRoman Nerd Font:pixelsize=16:style=Book,xft:file-icons:pixelsize=14' -fb 'xft:CodeNewRoman Nerd Font:pixelsize=18:style=Bold,xft:file-icons:pixelsize=14' -fi 'xft:CodeNewRoman Nerd Font:pixelsize=18:style=Italic,xft:file-icons:pixelsize=14' -fbi 'xft:CodeNewRoman Nerd Font:pixelsize=18:style=Italic,xft:file-icons:pixelsize=14'" ++ " -name subterm -e tmux"
    findSubTerm = resource =? "subterm"
    manageSubTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.382
        t = 0.618
        w = 1.0
        l = 0.0
    spawnSpotify = "spotify"
    findSpotify = resource =? "spotify"
    manageSpotify = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        t = 0.05
        w = 0.9
        l = 0.05
    spawnFirefox = "firefox && sleep 0.5 && xdotool getwindowfocus set_window --role 'scratchfirefox'; pause 0.3; compton-trans -c 80"
    findFirefox = stringProperty "WM_WINDOW_ROLE" =? "scratchfirefox"
    manageFirefox = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        t = 0.05
        w = 0.9
        l = 0.05
    spawnTop = "urxvtcd" ++ " -depth 32 -name scratchtop -e htop -d 6; compton-trans -c 95"
    findTop = resource =? "scratchtop"
    manageTop = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        t = 0.05
        w = 0.9
        l = 0.05
    spawnElfeed = "urxvtcd" ++ " -name scratchelfeed -e emacsclient -nw --socket-name=/tmp/emacs1000/server -e '(fn-elfeed-from-term)'"
    findElfeed = resource =? "scratchelfeed"
    manageElfeed = customFloating $ W.RationalRect l t w h
      where
        h = 0.36
        t = 0.1
        w = 0.55
        l = 0.4
    -- spawnShutter = "shutter"
    -- findShutter = resource =? "shutter"
    -- manageShutter = customFloating $ W.RationalRect l t w h
    --   where
    --     h = 0.5
    --     t = 0.1
    --     w = 0.5
    --     l = (1-w)/2



------------------------------------------------------------------------
-- * Window rules:

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

-- Managehook Helpers --------------
-- check: https://wiki.haskell.org/Xmonad/Config_archive/ivy-foster-xmonad.hs (!)
-- and:   https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Hooks-ManageHelpers.html#t:MaybeManageHook  (!)
doFlopDown :: ManageHook
doFlopDown = doF (W.focusUp . W.swapDown)

-- doFlopUp :: ManageHook
-- doFlopUp = doF (W.focus)

-- doFloatFocus :: ManageHook
-- doFloatFocus = ask >>= \w -> doF . W.float w . W.focusUp . snd =<< liftX (floatLocation w)

doFocus :: ManageHook
doFocus = doF (W.focusUp . W.swapUp)

doTopRightFloat :: ManageHook
doTopRightFloat = ask
                >>= \w -> doF . W.float w . position . snd
                =<< liftX (floatLocation w)
    where
    position (W.RationalRect _ _ w h) = W.RationalRect (1-w) 0.03 w h

doTopLeftFloat :: ManageHook
doTopLeftFloat = ask
                >>= \w -> doF . W.float w . position . snd
                =<< liftX (floatLocation w)
    where
    position (W.RationalRect _ _ w h) = W.RationalRect 0 0.03 w h

doBottomRightFloat :: ManageHook
doBottomRightFloat = ask
                    >>= \w -> doF . W.float w . position . snd
                    =<< liftX (floatLocation w)
    where
    position (W.RationalRect _ _ w h) = W.RationalRect (1-w) (1-h) w h

doBottomLeftFloat :: ManageHook
doBottomLeftFloat = ask
                    >>= \w -> doF . W.float w . position . snd
                    =<< liftX (floatLocation w)
    where
    position (W.RationalRect _ _ w h) = W.RationalRect 0 (1-h) w h


manageShutter :: ManageHook
manageShutter = composeOne
  [ resource =? "shutter" -?> doFlopDown ]

manageIres :: ManageHook
manageIres = composeOne
  [ stringProperty "_NET_WM_NAME" =? "TimelineGraphs" -?> doFloat
  , stringProperty "_NET_WM_NAME" =? "CarViewerOutgo" -?> doFloat
  , stringProperty "_NET_WM_NAME" =? "CarViewer"      -?> doFloat
  , stringProperty "_NET_WM_NAME" =? "CarViewerIngo"  -?> doFloat
  , stringProperty "_NET_WM_NAME" =? "StationViewer"  -?> doFloat
  , stringProperty "_NET_WM_NAME" =? "TimelineGraphs" -?> doFloat
  , stringProperty "_NET_WM_NAME" =? "IdvrWidget"     -?> doFloat
  ]


manageThunderbird :: ManageHook
manageThunderbird = composeOne
    [ stringProperty "WM_WINDOW_ROLE" =? "Msgcompose" -?> doFloat
    , stringProperty "WM_WINDOW_ROLE" =? "3pane"      -?> doShift "b_1" ]

myManageHook dir_system =  namedScratchpadManageHook (mScratchpads dir_system) <+> composeOne
    [ checkDock                     -?> doIgnore  -- equivalent to manageDocks
    , isDialog 		                -?> doF W.shiftMaster <+> doF W.swapDown
    , className =? "MPlayer"        -?> doFloat
    , className =? "Gimp"           -?> doFloat
    , className =? "Thunderbird"    -?> manageThunderbird <+> doFocus
    , resource  =? "desktop_window" -?> doIgnore
    , resource  =? "kdesktop"       -?> doIgnore
    -- , title     =? "FileLineEdit"   --> doFloat <+> doF W.swapUp
    -- , title     =? "CompletionList" --> doFloat <+> doF
    , title     =? "FileLineEdit"   -?> doFloat
    , title     =? "CompletionList" -?> doF W.swapDown
    , resource  =? "shutter"        -?> doFloat <+> doF W.swapDown
    , resource  =? "ires20"         -?> manageIres     -- shift ires windows to workspace called c_5
    , stringProperty "_NET_WM_NAME"  =? "Virtual Network Editor"  -?> doFloat
    , resource =? "QtCreatorTerm"    -?> doFloat
    , stringProperty "WM_WINDOW_ROLE" =? "geeqie" -?> doFloat
    , stringProperty "_NET_WM_NAME"  =? "Figure 1" -?> doFloat
    , stringProperty "WM_NAME"  =? "Password erforderlich" -?> doFloat
    , resource =? "zeal" -?> doFloat
    , resource =? "cmake-gui" -?> doFloat
    , resource =? "nautilus" -?> doFloat
    , resource =? "nemo" -?> doFloat
    , resource =? "tixati" -?> doFloat
    , stringProperty "WM_NAME" =? "Spotify" -?> doFloat
    , stringProperty "WM_CLASS" =? "spotify" -?> doFloat
    , stringProperty "_NET_WM_NAME" =? "Spotify" -?> doFloat
    , stringProperty "WM_NAME" =? "pyosd" -?> doFloat
    ]


------------------------------------------------------------------------
-- * Event handling

-- EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- myEventHook = XMonad.Layout.Fullscreen.fullscreenEventHook
-- mEventHook = ewmhDesktopsEventHook

mEventHook :: Event -> X All
mEventHook DestroyWindowEvent {ev_window = w} =
    do
        nextMatch History (return True)
        return $ (All False)
mEventHook _ = return $ (All True)

------------------------------------------------------------------------
-- ** Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- myLogHook xmproc_h = dynamicLogWithPP xmobarPP
--                      { ppOutput = hPutStrLn xmproc_h
--                      , ppTitle = xmobarColor "green" "" . shorten 50 -- first 50 characters of window title in title area
--                      }

-- myLogHook :: Handle -> X ()
-- myLogHook h = dynamicLogWithPP $ defaultPP

--     -- display current workspace as darkgrey on light grey (opposite of
--     -- default colors)
--     { ppCurrent         = dzenColor "#303030" "#909090" . pad

--     -- display other workspaces which contain windows as a brighter grey
--     , ppHidden          = dzenColor "#909090" "" . pad

--     -- display other workspaces with no windows as a normal grey
--     , ppHiddenNoWindows = dzenColor "#606060" "" . pad

--     -- display the current layout as a brighter grey
--     , ppLayout          = dzenColor "#909090" "" . pad

--     -- if a window on a hidden workspace needs my attention, color it so
--     , ppUrgent          = dzenColor "#ff0000" "" . pad . dzenStrip

--     -- shorten if it goes over 100 characters
--     , ppTitle           = shorten 100

--     -- no separator between workspaces
--     , ppWsSep           = ""

--     -- put a few spaces between each object
--     , ppSep             = "  "

--     -- output to the handle we were given as an argument
--     , ppOutput          = hPutStrLn h
--     }

mIconsDir = "/home/fthevissen/.xmonad/icons"

myLogHook :: Handle -> X ()

myLogHook h = dynamicLogWithPP $ defaultPP
    {
      ppCurrent              = showWorkspacesIcon "#ebac54" "#ebac54" "#1b1d1e"
    , ppVisible         = (\z -> "")
    , ppHidden          = (\z -> "")
      -- , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad
      -- , ppHiddenNoWindows   = showNamedWorkspaces
      -- , ppUrgent            =   dzenColor "#ff0000" "#1B1D1E" . pad
      -- , ppWsSep             =   " "
    , ppSep             =   " "
    , ppLayout          =   dzenColor "#ebac54" "#1B1D1E" .
                            (\x -> case x of
                                    "Maximize ResizableTall"             ->       "^i(" ++ mIconsDir ++ "/layout-tall.xbm)"
                                    "Maximize Mirror ResizableTall"      ->       "^i(" ++ mIconsDir ++ "/mtall.xbm)"
                                    "Maximize Full"                      ->       "^i(" ++ mIconsDir ++ "/layout-full.xbm)"
                                    "Maximize Grid"			->	"^i(" ++ mIconsDir ++ "/grid.xbm)"
                                    "Maximize Tabbed Simplest"		->	"^i(" ++ mIconsDir ++ "/layout-tabbed.xbm)"
                                    "Maximize imple Float"              ->       "~"
                                    _                           ->      x
                            )
    , ppTitle           =   (\x -> " ") . dzenEscape
    , ppOutput          =   hPutStrLn h
    }
  where
    showNamedWorkspaces wsId = if any (`elem` wsId) ['c'..'z']
                                       then pad wsId
                                       else ""
    showWorkspacesIcon fgActive fgIcon bg wsId =
      let
        -- (W.tag . W.workspace . W.current . windowset)
        -- currentWs :: X WorkspaceId
        -- currentWs = W.current . windowset
        -- currentWsEq1 = currentWs == 5
        -- currentWs :: W.StackSet
        -- currentWs = gets windowset
          -- W.current (gets windowset)
          -- ws <- W.current (gets windowset)
          -- putStrLn "hello"
          -- gets windowset
        --   ws <- gets (W.index . windowset)
        --   length ws
          -- ws
          -- ws <- gets windowset
          -- W.tag $ W.workspace $ W.current ws
           -- (withWindowSet $ return . W.currentTag)
        formatString offset activeWS icon =
          "^fg(" ++ fgActive ++ ")" ++ [intToDigit $ offset + digitToInt activeWS] ++ " ^fg(" ++ fgIcon ++ ")^i(" ++ mIconsDir ++ icon ++ ")"
        id2icon (a:_:n:_)
          | a == 'a' = formatString 0 n "/workspace_tl.xbm"
          | a == 'b' = formatString 2 n "/workspace_tr.xbm"
          | a == 'c' = formatString 4 n "/workspace_bl.xbm"
          | a == 'd' = formatString 6 n "/workspace_br.xbm"
          | otherwise = wsId
      in id2icon wsId


------------------------------------------------------------------------
-- ** Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
-- myStartupHook = return ()

------------------------------------------------------------------------
-- * main

myXMonadBar = "sleep 0.1; dzen2 -p -xs 2 -ta c -dock -e 'onstart=lower' -fn 'CodeNewRoman Nerd Font:pixelsize=11' -bg '#131a00'"
-- myStatusBar = "sleep 0.1; conky -c ~/.conkyrc | dzen2 -p -xs 1 -ta r -dock -e 'onstart=lower' -fn 'CodeNewRoman Nerd Font:pixelsize=12' -bg '#131a00'"
-- myIconTray  = "sleep 0.1; trayer --edge top --align right --SetDockType true --SetPartialStrut true \
--                \--expand true --width 5 --transparent true --alpha 255 --height 16 --distancefrom right --distance 10 --expand true &"

-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO()
main = do
  -- status bars / icon tray --
  spawn $ "kill `pgrep dzen`; kill `pgrep conky`; kill `pgrep trayer`"
  --spawn $ "stack exec taffybar"
  dzenproc <-  spawnPipe myXMonadBar
  -- spawn $ myStatusBar
  -- spawn $ myIconTray
  dir_system <- getEnv "DIR_SYSTEM"

  defaults_ <-
    -- XMonad.Actions.WindowNavigation:   hjkl window navigation and swapping
        withWindowNavigationKeys [((mod4Mask              , xK_k), WNGo   U),
                                  ((mod4Mask              , xK_h), WNGo   L),
                                  ((mod4Mask              , xK_j), WNGo   D),
                                  ((mod4Mask              , xK_l), WNGo   R),
                                  ((mod4Mask .|. shiftMask, xK_k), WNSwap U),
                                  ((mod4Mask .|. shiftMask, xK_h), WNSwap L),
                                  ((mod4Mask .|. shiftMask, xK_j), WNSwap D),
                                  ((mod4Mask .|. shiftMask, xK_l), WNSwap R)]
        $ defaults dzenproc dir_system

  xmonad $ withUrgencyHook NoUrgencyHook $ ewmh $ docks defaults_

defaults dzenproc dir_system =
  def {
          -- (Non-overriding fields in the default config will use defaults defined in
          -- xmonad/XMonad/Config.hs)
          terminal           = "urxvtcd",
          focusFollowsMouse  = False,
          clickJustFocuses   = False,
          borderWidth        = 1,
          modMask            = mod4Mask,  -- default: left alt, mod1Mask
          XMonad.workspaces         = myWorkspaces,
          workspaces = myWorkspaces,
          normalBorderColor  = "#1d1d1d",
          focusedBorderColor = "#1d1d1d",

          -- key bindings
          keys               = myKeys dir_system,
          mouseBindings      = myMouseBindings,

          -- hooks, layouts
          layoutHook         = myLayout,
          manageHook         = myManageHook dir_system,

          -- handleEventHook    = mEventHook >> ewmhDesktopsEventHook,
          -- handleEventHook 	= myServerModeeventHook,
          handleEventHook    = mconcat
                               [   ewmhDesktopsEventHook,
                                   fnServerModeEventHook    
                               ],
          logHook            = myLogHook dzenproc >> historyHook,
          startupHook        = myStartupHook
          }

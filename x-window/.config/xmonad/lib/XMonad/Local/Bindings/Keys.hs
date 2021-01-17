module XMonad.Local.Bindings.Keys
    ( xmKeys
    ) where

import qualified Data.Map as Map
    ( fromList
    )
import qualified Graphics.X11.ExtraTypes.XF86 as XF86
import System.Exit
    ( exitSuccess
    )

import XMonad
import qualified XMonad.StackSet as XMSS
import qualified XMonad.Actions.Submap as XMSM
    ( submap
    )
import XMonad.Actions.CycleWS
    ( shiftToPrev
    , shiftToNext
    , prevWS
    , nextWS
    )
import XMonad.Actions.CopyWindow
    ( copy
    , kill1
    , copyToAll
    , killAllOtherCopies
    )
import XMonad.Hooks.ManageDocks
    ( ToggleStruts (..)
    )
import XMonad.Layout.LayoutCombinators
    ( JumpToLayout (..)
    )
import XMonad.Layout.Spacing
    ( setScreenWindowSpacing
    , incWindowSpacing
    , decWindowSpacing
    )
import XMonad.Layout.WindowNavigation
    ( Navigate (..)
    )
import XMonad.Layout.ResizableTile
    ( MirrorResize (..)
    )
import XMonad.Util.NamedScratchpad
    ( namedScratchpadAction
    )
import XMonad.Util.Types
    ( Direction2D (..)
    )

import XMonad.Local.Manage.Util
    ( xmScratchpads
    , xmBigRect
    )
import XMonad.Local.Config.Workspace
    ( xmWorkspaces
    )
import XMonad.Local.Bindings.Util
    ( Direction (..)
    , moveFloating
    , resizeFloating
    , terminalFromConf
    , inTerminalFromConf
    )
import XMonad.Local.Bindings.Bind
    ( Binder
    , (|/-)
    , (^>)
    , (...)
    , bind
    -- , bindAlias
    , bindZip
    , getBindings
    )

xmKeys :: KeyMask -> Binder ()
xmKeys mask = do
    --------------------------------------------------------------------------
    -- Left side characters
    bind $ mask ... xK_q
      |/- "recompile and restart xmonad"
        ^> spawn "xmonad --recompile; xmonad --restart"
    bind $ mask .|. shiftMask ... xK_q
      |/- "kill focused window"
        ^> kill
    bind $ mask ... xK_w
      |/- "switch to physical/Xinerama screen 0"
        ^> screenWorkspace 0 >>= flip whenJust (windows . XMSS.view)
    bind $ mask ... xK_e
      |/- "switch to physical/Xinerama screen 1"
        ^> screenWorkspace 1 >>= flip whenJust (windows . XMSS.view)
    bind $ mask ... xK_r
      |/- "switch to physical/Xinerama screen 2"
        ^> screenWorkspace 2 >>= flip whenJust (windows . XMSS.view)
    bind $ mask .|. shiftMask ... xK_w
      |/- "move focused window to physical/Xinerama screen 0"
        ^> screenWorkspace 0 >>= flip whenJust (windows . XMSS.shift)
    bind $ mask .|. shiftMask ... xK_e
      |/- "move focused window to physical/Xinerama screen 1"
        ^> screenWorkspace 1 >>= flip whenJust (windows . XMSS.shift)
    bind $ mask .|. shiftMask ... xK_r
      |/- "move focused window to physical/Xinerama screen 0"
        ^> screenWorkspace 2 >>= flip whenJust (windows . XMSS.shift)
    bind $ mask ... xK_t
      |/- "move focused floating window back into layout"
        ^> withFocused $ windows . XMSS.sink
    bind $ mask .|. shiftMask ... xK_t
      |/- "float and center the focused window"
        ^> withFocused $ windows . flip XMSS.float xmBigRect
    bind $ mask ... xK_a
      |/- "spawn default terminal"
        ^> spawn =<< terminalFromConf
    bind $ mask ... xK_s
      |/- "increase gap spacing"
        ^> incWindowSpacing 1
    bind $ mask .|. shiftMask ... xK_s
      |/- "decrease gap spacing"
        ^> decWindowSpacing 1
    bind $ mask .|. controlMask ... xK_s
      |/- "remove gaps"
        ^> setScreenWindowSpacing 0
    bind $ mask ... xK_d
      |/- "spawn emacs"
        ^> spawn "emacs"
    bind $ mask .|. shiftMask ... xK_d
      |/- "spawn file explorer"
        ^> spawn "nautilus"
    bind $ mask ... xK_f
      |/- "spawn internet browser"
        ^> spawn "vivaldi-stable"
    bind $ mask .|. shiftMask ... xK_f
      |/- "tabbed surf"
        ^> spawn "surf-open"
    bind $ mask ... xK_c
      |/- "copy focused window to all workspaces"
        ^> windows copyToAll
    bind $ mask .|. shiftMask ... xK_c
      |/- "kill all the copy of the focused window"
        ^> killAllOtherCopies
    bind $ mask .|. controlMask ... xK_c
      |/- "remove focused window from this workspace"
        ^> kill1
    bind $ mask ... xK_b
      |/- "toggle covering of docks, status bars ..."
        ^> sendMessage ToggleStruts
    --------------------------------------------------------------------------
    -- Right side characters
    bind $ mask .|. controlMask .|. shiftMask ... xK_y
      |/- "spawn terminal scratchpad"
        ^> namedScratchpadAction xmScratchpads "yakuake"
    bind $ mask ... xK_u
      |/- "spawn dmenu"
        ^> spawn "dmenu_run"
    bind $ mask .|. shiftMask... xK_u
      |/- "spawn rofi"
        ^> spawn "rofi -modi drun,run,combi -show combi"
    bind $ mask .|. controlMask ... xK_u
      |/- "spawn xmenu-apps"
        ^> spawn "xmenu-apps"
    bind $ mask .|. controlMask .|. shiftMask ... xK_o
      |/- "spawn emacs org-agenda scratchpad"
        ^> namedScratchpadAction xmScratchpads "orgenda"
    bind $ mask ... xK_p
      |/- "submap for mpc and mpv players"
        ^> XMSM.submap . Map.fromList $
            [ ((0, xK_h),     spawn "mpc prev")
            , ((0, xK_l),     spawn "mpc next")
            , ((0, xK_j),     spawn "mpc play")
            , ((0, xK_k),     spawn "mpc pause")
            , ((0, xK_space), spawn "mpc toggle")
            , ((0, xK_t),     spawn "mpv_bulk_toggle")
            , ((0, xK_q),     spawn "mpv_bulk_quit")
            ]
    bind $ mask ... xK_k
      |/- "focus previous window"
        ^> windows XMSS.focusUp
    bind $ mask ... xK_j
      |/- "focus next window"
        ^> windows XMSS.focusDown
    bind $ mask .|. shiftMask ... xK_k
      |/- "swap focused window with previous"
        ^> windows XMSS.swapDown
    bind $ mask .|. shiftMask ... xK_j
      |/- "swap focused window with next"
        ^> windows XMSS.swapUp
    bind $ mask ... xK_h
      |/- "shrink master pane"
        ^> sendMessage Shrink
    bind $ mask ... xK_l
      |/- "expand master pane"
        ^> sendMessage Expand
    bind $ mask .|. shiftMask ... xK_h
      |/- "shrink focused slave window"
        ^> sendMessage MirrorShrink
    bind $ mask .|. shiftMask ... xK_l
      |/- "expand focused slave window"
        ^> sendMessage MirrorExpand
    bind $ mask .|. controlMask ... xK_k
      |/- "focus the window to the north"
        ^> sendMessage $ Go U
    bind $ mask .|. controlMask ... xK_j
      |/- "focuse the window to the south"
        ^> sendMessage $ Go D
    bind $ mask .|. controlMask ... xK_h
      |/- "focus the window to the west"
        ^> sendMessage $ Go L
    bind $ mask .|. controlMask ... xK_l
      |/- "focus the window to the east"
        ^> sendMessage $ Go R
    bind $ mask .|. controlMask .|. shiftMask ... xK_k
      |/- "move focused window to the north"
        ^> sendMessage $ Move U
    bind $ mask .|. controlMask .|. shiftMask ... xK_j
      |/- "move focused window to the south"
        ^> sendMessage $ Move D
    bind $ mask .|. controlMask .|. shiftMask ... xK_h
      |/- "move focused window to the west"
        ^> sendMessage $ Move L
    bind $ mask .|. controlMask .|. shiftMask ... xK_l
      |/- "move focused window to the east"
        ^> sendMessage $ Move R
    bind $ mask ... xK_m
      |/- "focus the master window"
        ^> windows XMSS.focusMaster
    bind $ mask .|. shiftMask ... xK_m
      |/- "swap focused window with master"
        ^> windows XMSS.swapMaster
    bind $ mask .|. controlMask .|. shiftMask ... xK_m
      |/- "spawn cmus scratchpad"
        ^> namedScratchpadAction xmScratchpads "cmus"
    --------------------------------------------------------------------------
    -- Extra Keys
    bind $ mask ... xK_comma
      |/- "increment number of windows in master pane"
        ^> sendMessage $ IncMasterN 1
    bind $ mask ... xK_period
      |/- "decrement number of windows in master pane"
        ^> sendMessage $ IncMasterN (-1)
    bind $ mask ... xK_bracketleft
      |/- "go to previous workspace"
        ^> prevWS
    bind $ mask ... xK_bracketright
      |/- "go to next workspace"
        ^> nextWS
    bind $ mask .|. shiftMask ... xK_bracketleft
      |/- "move focused window to previous workspace"
        ^> shiftToPrev >> prevWS
    bind $ mask .|. shiftMask ... xK_bracketright
      |/- "move focused window to next workspace"
        ^> shiftToNext >> nextWS
    bind $ mask ... xK_Tab
      |/- "select the next layout"
        ^> sendMessage NextLayout
    bind $ mask .|. shiftMask ... xK_Tab
      |/- "reset layout on current workspaces"
        ^> setLayout =<< asks (layoutHook . config)
    bind $ mask ... xK_space
      |/- "select the monocle layout"
        ^> sendMessage $ JumpToLayout "Monocle"
    bind $ mask .|. shiftMask ... xK_space
      |/- "select the floating layout"
        ^> sendMessage $ JumpToLayout "Float >>="
    bind $ mask ... xK_Return
      |/- "spawn default terminal"
        ^> spawn =<< terminalFromConf
    bind $ mask .|. shiftMask ... xK_Return
      |/- "spawn secondary terminal"
        ^> spawn "kitty"
    bind $ mask .|. shiftMask ... xK_BackSpace
      |/- "Lock the screen"
        ^> spawn "i3lock -i ~/.config/xmonad/screenlocker.png -t -f -e"
    bind $ mask ... xK_Delete
      |/- "spawn shutdown menu"
        ^> spawn "xmenu-shutdown"
    bind $ mask .|. shiftMask ... xK_Delete
      |/- "logout from XMonad"
        ^> io exitSuccess
    bind $ mask ... xK_Print
      |/- "take fullscreen screenshot"
        ^> spawn "scrot ~/Pictures/Screenshots/%Y-%m-%d-%T-screenshoot.png; notify-send -i photo 'Taken fullscreen screenshot'"
    bind $ mask .|. shiftMask ... xK_Print
      |/- "select a rectangular region to take a screenshot"
        ^> spawn "import ~/Pictures/Screenshots/$(date +%Y-%m-%d-%T)screenshoot.png; notify-send -i photo 'Saved to ~/Pictures/Screenshots'"
    -- Arrows
    bind $ mask ... xK_Up
      |/- "move floating window up"
        ^> withFocused $ moveFloating FU
    bind $ mask ... xK_Down
      |/- "move floating window down"
        ^> withFocused $ moveFloating FD
    bind $ mask ... xK_Left
      |/- "move floating window left"
        ^> withFocused $ moveFloating FL
    bind $ mask ... xK_Right
      |/- "move floating window right"
        ^> withFocused $ moveFloating FR
    bind $ mask .|. shiftMask ... xK_Up
      |/- "shrink floating window vertically"
        ^> withFocused $ resizeFloating FU
    bind $ mask .|. shiftMask ... xK_Down
      |/- "expand floating window vertically"
        ^> withFocused $ resizeFloating FD
    bind $ mask .|. shiftMask ... xK_Left
      |/- "shrink floating window horizontally"
        ^> withFocused $ resizeFloating FL
    bind $ mask .|. shiftMask ... xK_Right
      |/- "expand floating window horizontally"
        ^> withFocused $ resizeFloating FR
    bind $ mask .|. controlMask .|. shiftMask ... xK_Up
      |/- "swap focused window with north"
        ^> sendMessage $ Swap U
    bind $ mask .|. controlMask .|. shiftMask ... xK_Down
      |/- "swap focused window with south"
        ^> sendMessage $ Swap D
    bind $ mask .|. controlMask .|. shiftMask ... xK_Left
      |/- "swap focused window with west"
        ^> sendMessage $ Swap L
    bind $ mask .|.controlMask .|. shiftMask ... xK_Right
      |/- "swap focused window with east"
        ^> sendMessage $ Swap R
    -- Numbers
    bindZip ((mask ...) <$> [ xK_1 .. xK_9 ])
            (("go to workspace " <>) . pure <$> [ '1' .. '9' ])
            (windows . XMSS.greedyView <$> xmWorkspaces)
    bindZip ((mask .|. shiftMask ...) <$> [ xK_1 .. xK_9 ])
            (("move focused window to workspace " <>) . pure <$> [ '1' .. '9' ])
            (windows . XMSS.shift <$> xmWorkspaces)
    bindZip ((mask .|. controlMask .|. shiftMask ...) <$> [ xK_1 .. xK_9 ])
            (("copy focused window to workspace " <>) . pure <$> [ '1' .. '9' ])
            (windows . copy <$> xmWorkspaces)
    -- Fun Keys and XF86 Keys
    bind $ mask ... xK_F1
      |/- "binding documentation"
        ^> do doc <- getBindings
              term <- terminalFromConf
              spawn $ term <> " -n keysheet -t keysheet sh -c \"echo '" <> doc <> "' | less\""
    bind $ noModMask ... XF86.xF86XK_AudioMute
      |/- "toggle mute/unmute audio"
        ^> spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"
    bind $ noModMask ... XF86.xF86XK_AudioLowerVolume
      |/- "decrease volume"
        ^> spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"
    bind $ noModMask ... XF86.xF86XK_AudioRaiseVolume
      |/- "increase volume"
        ^> spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"
    -- XF86.xF86XK_AudioMicMute
    bind $ noModMask ... XF86.xF86XK_MonBrightnessDown
      |/- "decrease screen brightness"
        ^> spawn "xbacklight -dec 5"
    bind $ noModMask ... XF86.xF86XK_MonBrightnessUp
      |/- "increase screen brightness"
        ^> spawn "xbacklight -inc 5"
    bind $ noModMask ... XF86.xF86XK_Display
      |/- "configure monitor setup"
        ^> spawn "monitor_handler"
    -- XF86.xF86XK_Tools
    bind $ noModMask ... XF86.xF86XK_Search
      |/- "spawn ranger"
        ^> spawn =<< inTerminalFromConf "ranger"
    -- XF86.xF86XK_LaunchA
    bind $ noModMask ... XF86.xF86XK_Explorer
      |/- "spawn file explorer"
        ^> spawn "nautilus"
    bind $ noModMask ... XF86.xF86XK_Calculator
      |/- "spawn calculator"
        ^> spawn =<< inTerminalFromConf "ghci"
    -- XF86.xF86XK_HomePage

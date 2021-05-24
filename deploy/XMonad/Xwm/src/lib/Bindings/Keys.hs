module Bindings.Keys
    ( xwmKeys
    ) where



import qualified Data.Map as Map (fromList)
import qualified Graphics.X11.ExtraTypes.XF86 as XF86
import System.Exit (exitSuccess)

import XMonad
import qualified XMonad.StackSet as XMSS

import XMonad.Actions.CopyWindow (copy, kill1, copyToAll, killAllOtherCopies)
import XMonad.Actions.CycleWS
    ( Direction1D(Next, Prev)
    , WSType(NonEmptyWS, WSIs)
    , moveTo
    , shiftTo
    , toggleWS'
    , prevScreen
    , nextScreen
    , shiftPrevScreen
    , shiftNextScreen
    , swapPrevScreen
    , swapNextScreen
    )
import XMonad.Actions.Promote (promote)
import XMonad.Actions.FloatKeys (keysMoveWindow, keysResizeWindow)
import qualified XMonad.Actions.Submap as XMSM (submap)

import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Hooks.UrgencyHook (focusUrgent)

import XMonad.Layout.LayoutCombinators (JumpToLayout (..))
import XMonad.Layout.ResizableTile (MirrorResize(..))
import XMonad.Layout.Spacing (setScreenWindowSpacing, incScreenWindowSpacing, decScreenWindowSpacing)
import XMonad.Layout.WindowNavigation (Navigate (..))

import XMonad.Prompt.Layout (layoutPrompt)
import XMonad.Prompt.Man (manPrompt)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Window
    ( allWindows
    , windowPrompt
    , wsWindows
    , WindowPrompt(Goto, Bring)
    )
import XMonad.Prompt.XMonad (xmonadPrompt)

import XMonad.Util.Types (Direction2D (..))
import XMonad.Util.NamedScratchpad (namedScratchpadAction)
import XMonad.Util.Ungrab (unGrab)

import Bindings.Binder
    ( Binder
    , (|/-)
    , (^>)
    , (...)
    , bind
    , bindZip
    , getBindings
    )
import Config.Workspaces (xwmWorkspaces)
import Manage.Util
    ( xwmBigRect
    , xwmMedRect
    , xwmSmallRect
    , xwmUpLeftRect
    , xwmUpRightRect
    , xwmDownLeftRect
    , xwmDownRightRect
    , terminalFromConf
    , inTerminalFromConf
    , xwmSPDs
    )
import Prompt.Config (xwmXpConfig)



data Direction =
    FL
    | FD
    | FU
    | FR

direction :: Direction -> D
direction d = (dx , dy)
  where
    (dx , dy) =
        case d of
            FL -> (-pixel , 0)
            FD -> (0 , pixel)
            FU -> (0 , -pixel)
            FR -> (pixel , 0)
    pixel = 20

moveFloat :: Direction -> Window -> X ()
moveFloat d = keysMoveWindow (direction d)

resizeFloat :: Direction -> Window -> X ()
resizeFloat d = keysResizeWindow (direction d) (0 , 0)

wsNonNSP :: WSType
wsNonNSP = WSIs $ return (\ws -> XMSS.tag ws /= "NSP")


xwmKeys :: KeyMask -> Binder ()
xwmKeys mask = do
    ---------------------------------------------------------------------------
        -- Left side characters
    bind $ mask ... xK_q
        |/- "Restart xmonad"
        ^> restart "xwm" True
    bind $ mask .|. shiftMask ... xK_q
        |/- "Kill focused client"
        ^> kill
    bind $ mask ... xK_w
        |/- "Switch to the next physical/Xinerama screen"
        ^> prevScreen
    bind $ mask ... xK_e
        |/- "Switch to the next physical/Xinerama screen"
        ^> nextScreen
    bind $ mask .|. shiftMask ... xK_w
        |/- "Move focused client to the next physical/Xinerama screen"
        ^> shiftPrevScreen
    bind $ mask .|. shiftMask ... xK_e
        |/- "Move focused client to the next physical/Xinerama screen"
        ^> shiftNextScreen
    bind $ mask .|. controlMask ... xK_w
        |/- "Swap current physical/Xinerama screen with the previous"
        ^> swapPrevScreen
    bind $ mask .|. controlMask ... xK_e
        |/- "Swap current physical/Xinerama screen with the next"
        ^> swapNextScreen
    bind $ mask ... xK_r
        |/- "Spawn tabbed zathura"
        ^> spawn "tabbed -c zathura -e"
    bind $ mask ... xK_t
        |/- "Push focused client back into tiling"
        ^> withFocused $ windows . XMSS.sink
    bind $ mask .|. shiftMask ... xK_t
        |/- "Float and center the focused window with quite big dimension"
        ^> withFocused $ windows . flip XMSS.float xwmBigRect
    bind $ mask .|. controlMask ... xK_t
        |/- "Float and center the focused window"
        ^> withFocused $ windows . flip XMSS.float xwmMedRect
    bind $ mask .|. shiftMask .|. controlMask ... xK_t
        |/- "Float and center the focused window with quite small dimension"
        ^> withFocused $ windows . flip XMSS.float xwmSmallRect
    bind $ mask ... xK_a
        |/- "Spawn the default terminal"
        ^> spawn =<< terminalFromConf
    bind $ mask .|. shiftMask ... xK_a
        |/- "Spawn the secondary terminal"
        ^> spawn "st"
    bind $ mask .|. controlMask ... xK_a
        |/- "Spawn a tabbed secondary terminal"
        ^> spawn "tabbed -c -r 2 st -w ''"
    bind $ mask ... xK_s
        |/- "Increase screen and windows spacing"
        ^> incScreenWindowSpacing 1
    bind $ mask .|. shiftMask ... xK_s
        |/- "Decrease screen and windows spacing"
        ^> decScreenWindowSpacing 1
    bind $ mask .|. controlMask ... xK_s
        |/- "Remove screen and windows spacing"
        ^> setScreenWindowSpacing 0
    bind $ mask ... xK_d
        |/- "Spawn emacs"
        ^> spawn "emacs"
    bind $ mask ... xK_f
        |/- "Spawn web browser"
        ^> spawn "vivaldi-stable"
    bind $ mask .|. shiftMask ... xK_f
        |/- "Spawn secondary web browser"
        ^> spawn "surf-open"
    bind $ mask ... xK_b
        |/- "Toggle the status bar gap"
        ^> sendMessage ToggleStruts
    ---------------------------------------------------------------------------
        -- Right side characters
    bind $ mask .|. shiftMask .|. controlMask ... xK_y
        |/- "Terminal scratchpad"
        ^> namedScratchpadAction xwmSPDs "Yakuake"
    bind $ mask ... xK_u
        |/- "Spawn dmenu launcher"
        ^> spawn "dmenu_run"
    bind $ mask .|. shiftMask ... xK_u
        |/- "Spawn rofi launcher"
        ^> spawn "rofi -modi drun,run,combi -show combi"
    bind $ mask .|. controlMask ... xK_u
        |/- "Spawn xmenu launcher"
        ^> spawn "xmenu-apps"
    bind $ mask .|. shiftMask .|. controlMask ... xK_o
        |/- "Emacs-Org-mode scratchpad"
        ^> namedScratchpadAction xwmSPDs "Orgenda"
    bind $ mask ... xK_p
        |/- "Submap for mpc and mpv players"
        ^> XMSM.submap . Map.fromList $
            [ ((noModMask, xK_h),     spawn "mpc prev")
            , ((noModMask, xK_l),     spawn "mpc next")
            , ((noModMask, xK_j),     spawn "mpc play")
            , ((noModMask, xK_k),     spawn "mpc pause")
            , ((noModMask, xK_space), spawn "mpc toggle")
            , ((noModMask, xK_r),     spawn "mpc repeat")
            , ((shiftMask, xK_r),     spawn "mpc consume")
            , ((noModMask, xK_z),     spawn "mpc random")
            , ((noModMask, xK_y),     spawn "mpc single")
            , ((noModMask, xK_q),     spawn "mpv_bulk_quit")
            , ((noModMask, xK_t),     spawn "mpv_bulk_toggle")
            , ((noModMask, xK_q),     spawn "mpv_bulk_quit")
            ]
    bind $ mask .|. shiftMask ... xK_p
        |/- "Submap for XMonad prompt"
        ^> XMSM.submap . Map.fromList $
            [ ((noModMask, xK_b), windowPrompt xwmXpConfig Bring allWindows)
            , ((noModMask, xK_g), windowPrompt xwmXpConfig Goto wsWindows)
            , ((shiftMask, xK_g), windowPrompt xwmXpConfig Goto allWindows)
            , ((noModMask, xK_l), layoutPrompt xwmXpConfig)
            , ((noModMask, xK_m), manPrompt xwmXpConfig)
            , ((noModMask, xK_s), shellPrompt xwmXpConfig)
            , ((noModMask, xK_x), xmonadPrompt xwmXpConfig)
            ]
    bind $ mask ... xK_k
        |/- "Move focus to the previous window"
        ^> windows XMSS.focusUp
    bind $ mask ... xK_j
        |/- "Move focus to the next window"
        ^> windows XMSS.focusDown
    bind $ mask .|. shiftMask ... xK_k
        |/- "Swap the focused window with the previous window"
        ^> windows XMSS.swapUp
    bind $ mask .|. shiftMask ... xK_j
        |/- "Swap the focused window with the next window"
        ^> windows XMSS.swapDown
    bind $ mask ... xK_h
        |/- "Shrink the master area"
        ^> sendMessage Shrink
    bind $ mask ... xK_l
        |/- "Expand the master area"
        ^> sendMessage Expand
    bind $ mask .|. shiftMask ... xK_h
        |/- "Shrink area of the focused client in the focused area"
        ^> sendMessage MirrorShrink
    bind $ mask .|. shiftMask ... xK_l
        |/- "Expand area of the focused client in the focused area"
        ^> sendMessage MirrorExpand
    bind $ mask .|. controlMask ... xK_k
        |/- "Focus the window to the north"
        ^> sendMessage $ Go U
    bind $ mask .|. controlMask ... xK_j
        |/- "Focus the window to the south"
        ^> sendMessage $ Go D
    bind $ mask .|. controlMask ... xK_h
        |/- "Focus the window to the west"
        ^> sendMessage $ Go L
    bind $ mask .|. controlMask ... xK_l
        |/- "Focus the window to the east"
        ^> sendMessage $ Go R
    bind $ mask .|. controlMask .|. shiftMask ... xK_k
        |/- "Move focused window to the north"
        ^> sendMessage $ Move U
    bind $ mask .|. controlMask .|. shiftMask ... xK_j
        |/- "Move focused window to the south"
        ^> sendMessage $ Move D
    bind $ mask .|. controlMask .|. shiftMask ... xK_h
        |/- "Move focused window to the west"
        ^> sendMessage $ Move L
    bind $ mask .|. controlMask .|. shiftMask ... xK_l
        |/- "Move focused window to the east"
        ^> sendMessage $ Move R
    bind $ mask .|. mod1Mask ... xK_k
        |/- "Swap focused window with north"
        ^> sendMessage $ Swap U
    bind $ mask .|. mod1Mask ... xK_j
        |/- "Swap focused window with south"
        ^> sendMessage $ Swap D
    bind $ mask .|. mod1Mask ... xK_h
        |/- "Swap focused window with west"
        ^> sendMessage $ Swap L
    bind $ mask .|. mod1Mask ... xK_l
        |/- "Swap focused window with east"
        ^> sendMessage $ Swap R
    bind $ mask ... xK_n
        |/- "Toggle the previously displayed workspace"
        ^> toggleWS' ["NSP"]
    bind $ mask .|. shiftMask ... xK_n
        |/- "Resize viewed windows to the correct size"
        ^> refresh
    bind $ mask .|. controlMask ... xK_n
        |/- "Focus the most recently urgent window"
        ^> focusUrgent
    bind $ mask .|. shiftMask .|. controlMask ... xK_n
        |/- "Ncmpcpp scratchpad"
        ^> namedScratchpadAction xwmSPDs "Ncmpcpp"
    bind $ mask ... xK_m
        |/- "Move focus to the master window"
        ^> windows XMSS.focusMaster
    bind $ mask .|. shiftMask ... xK_m
        |/- "Swap the focused window and the master window"
        ^> promote
    bind $ mask .|. shiftMask .|. controlMask ... xK_m
        |/- "Cmus scratchpad"
        ^> namedScratchpadAction xwmSPDs "Cmus"
    ---------------------------------------------------------------------------
        -- Surrounding keys
    bind $ mask ... xK_Tab
        |/- "Cycle forward through the available layout algorithms"
        ^> sendMessage NextLayout
    bind $ mask .|. shiftMask ... xK_Tab
        |/-  "Reset the layouts on the current workspace to default"
        ^> setLayout =<< asks (layoutHook . config)
    bind $ mask ... xK_space
        |/- "Select the Tabbed layout"
        ^> sendMessage $ JumpToLayout "Tabbed"
    bind $ mask .|. shiftMask ... xK_space
        |/- "Select the Float layout"
        ^> sendMessage $ JumpToLayout "Float"
    bind $ mask .|. controlMask ... xK_space
        |/- "Select the ResizableThreeCol layout"
        ^> sendMessage $ JumpToLayout "ResizableThreeCol"
    bind $ mask ... xK_Delete
        |/- "Spawn shutdown menu"
        ^> spawn "xmenu-shutdown"
    bind $ mask .|. shiftMask ... xK_Delete
        |/- "Quit xmonad"
        ^> io exitSuccess
    bind $ mask .|. shiftMask ... xK_BackSpace
        |/- "Screenlocker"
        ^> spawn "slock"
    bind $ mask ... xK_Return
        |/- "Spawn the default terminal"
        ^> spawn =<< terminalFromConf
    bind $ mask .|. shiftMask ... xK_Return
        |/- "Spawn the secondary terminal"
        ^> spawn "st"
    bind $ mask .|. controlMask ... xK_Return
        |/- "Spawn a tabbed secondary terminal"
        ^> spawn "tabbed -c -r 2 st -w ''"
    bind $ mask ... xK_bracketleft
        |/- "Go to previous workspace"
        ^> moveTo Prev wsNonNSP
    bind $ mask ... xK_bracketright
        |/- "Go to next workspace"
        ^> moveTo Next wsNonNSP
    bind $ mask .|. shiftMask ... xK_bracketleft
        |/- "Move focused window to previous workspace"
        ^> shiftTo Prev wsNonNSP >> moveTo Prev wsNonNSP
    bind $ mask .|. shiftMask ... xK_bracketright
        |/- "Move focused window to next workspace"
        ^> shiftTo Next wsNonNSP >> moveTo Next wsNonNSP
    bind $ mask ... xK_comma
        |/- "Increment the number of windows in the master area"
        ^> sendMessage $ IncMasterN 1
    bind $ mask ... xK_period
        |/- "Deincrement the number of windows in the master area"
        ^> sendMessage $ IncMasterN (-1)
    bind $ mask .|. shiftMask ... xK_comma
        |/- "Go to previous non-empty workspace"
        ^> moveTo Prev NonEmptyWS
    bind $ mask .|. shiftMask ... xK_period
        |/- "Go to next non-empty workspace"
        ^> moveTo Next NonEmptyWS
    bind $ mask ... xK_Print
        |/- "Take fullscreen screenshot"
        ^> unGrab >> spawn (
            "scrot ~/Pictures/Screenshots/%Y-%m-%d-%T-screenshoot.png;"
            ++ "notify-send -i photo 'Taken fullscreen screenshot'")
    bind $ mask .|. shiftMask ... xK_Print
        |/- "Select a rectangular region to take a screenshot"
        ^> unGrab >> spawn (
            "scrot -s -l style=dash ~/Pictures/Screenshots/$(date +%Y-%m-%d-%T)screenshoot.png;"
            ++ "notify-send -i photo 'Saved to ~/Pictures/Screenshots'")
    ---------------------------------------------------------------------------
        -- Arrow keys
    bind $ mask ... xK_Up
        |/- "Move floating window up"
        ^> withFocused $ moveFloat FU
    bind $ mask ... xK_Down
        |/- "Move floating window down"
        ^> withFocused $ moveFloat FD
    bind $ mask ... xK_Left
        |/- "Move floating window left"
        ^> withFocused $ moveFloat FL
    bind $ mask ... xK_Right
        |/- "Move floating window right"
        ^> withFocused $ moveFloat FR
    bind $ mask .|. shiftMask ... xK_Up
        |/- "Shrink floating window vertically"
        ^> withFocused $ resizeFloat FU
    bind $ mask .|. shiftMask ... xK_Down
        |/- "Expand floating window vertically"
        ^> withFocused $ resizeFloat FD
    bind $ mask .|. shiftMask ... xK_Left
        |/- "Shrink floating window horizontally"
        ^> withFocused $ resizeFloat FL
    bind $ mask .|. shiftMask ... xK_Right
        |/- "Expand floating window horizontally"
        ^> withFocused $ resizeFloat FR
    bind $ mask .|. controlMask ... xK_Left
        |/- "Float and move to the down-left corner the focused window"
        ^> withFocused $ windows . flip XMSS.float xwmDownLeftRect
    bind $ mask .|. controlMask ... xK_Down
        |/- "Float and move to the down-right corner the focused window"
        ^> withFocused $ windows . flip XMSS.float xwmDownRightRect
    bind $ mask .|. controlMask ... xK_Right
        |/- "Float and move to the up-right corner the focused window"
        ^> withFocused $ windows . flip XMSS.float xwmUpRightRect
    bind $ mask .|. controlMask ... xK_Up
        |/- "Float and move to the up-left corner the focused window"
        ^> withFocused $ windows . flip XMSS.float xwmUpLeftRect
    ---------------------------------------------------------------------------
        -- Numbers
    bindZip ((mask ...) <$> [ xK_1 .. xK_9 ])
        (("Switch to workspace " <>) . pure <$> [ '1' .. '9' ])
        (windows . XMSS.greedyView <$> xwmWorkspaces)
    bindZip ((mask .|. shiftMask ...) <$> [ xK_1 .. xK_9 ])
        (("Move focused client to workspace " <>) . pure <$> [ '1' .. '9' ])
        (windows . XMSS.shift <$> xwmWorkspaces)
    bindZip ((mask .|. controlMask .|. shiftMask ...) <$> [ xK_1 .. xK_9 ])
        (("Copy focused window to workspace " <>) . pure <$> [ '1' .. '9' ])
        (windows . copy <$> xwmWorkspaces)
    bind $ mask ... xK_0
        |/- "Copy focused window to all workspaces"
        ^> windows copyToAll
    bind $ mask .|. shiftMask ... xK_0
        |/- "Remove focused window from this workspace"
        ^> kill1
    bind $ mask .|. controlMask ... xK_0
        |/- "Kill all the copy of the focused window"
        ^> killAllOtherCopies
    ---------------------------------------------------------------------------
        -- Fn and XF86 keys
    bind $ mask ... xK_F1
        |/- "Binding documentation"
        ^> do doc <- getBindings
              term <- terminalFromConf
              spawn $
                term
                <> " --name keysheet --title keysheet sh -c \"echo '"
                <> doc
                <> "' | less\""
    bind $ noModMask ... XF86.xF86XK_AudioMute
        |/- "Toggle mute/unmute audio"
        ^> spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"
    bind $ noModMask ... XF86.xF86XK_AudioLowerVolume
        |/- "Decrease volume"
        ^> spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"
    bind $ noModMask ... XF86.xF86XK_AudioRaiseVolume
        |/- "Increase volume"
        ^> spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"
    bind $ noModMask ... XF86.xF86XK_MonBrightnessDown
        |/- "Decrease screen brightness"
        ^> spawn "xbacklight -dec 5"
    bind $ noModMask ... XF86.xF86XK_MonBrightnessUp
        |/- "Increase screen brightness"
        ^> spawn "xbacklight -inc 5"
    bind $ noModMask ... XF86.xF86XK_Display
        |/- "Configure monitor setup"
        ^> spawn "monitor_handler"
    bind $ noModMask ... XF86.xF86XK_Search
        |/- "Spawn nnn"
        ^> spawn =<< inTerminalFromConf "nnn"
    bind $ noModMask ... XF86.xF86XK_Explorer
        |/- "Spawn web browser"
        ^> spawn "vivaldi-stable"
    bind $ noModMask ... XF86.xF86XK_Calculator
        |/- "Spawn calculator"
        ^> spawn =<< inTerminalFromConf "ghci"

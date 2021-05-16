
module Bindings.Keys
    ( xwmKeys
    ) where



import qualified Data.Map as Map
import qualified Graphics.X11.ExtraTypes.XF86 as XF86
import System.Exit (exitSuccess)

import XMonad
import qualified XMonad.StackSet as XMSS

import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Layout.ResizableTile (MirrorResize(..))
import XMonad.Util.NamedScratchpad (namedScratchpadAction)
import XMonad.Util.Ungrab (unGrab)

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



xwmKeys :: XConfig Layout -> Map.Map (KeyMask, KeySym) (X ())
xwmKeys conf@XConfig {XMonad.modMask = winKey} = Map.fromList $
    [
    ---------------------------------------------------------------------------
        -- Left side characters
      ((winKey,                               xK_q),
        -- Recompile and restart xmonad
        spawn "xwm --recompile" >> restart "xwm" True)
    , ((winKey .|. shiftMask,                 xK_q),
        -- Kill focused client
        kill)
    , ((winKey,                               xK_t),
        -- Push focused client back into tiling
        withFocused $ windows . XMSS.sink)
    , ((winKey .|. shiftMask,                 xK_t),
        -- Float and center the focused window with quite big dimension
        withFocused $ windows . flip XMSS.float xwmBigRect)
    , ((winKey .|. controlMask,               xK_t),
        -- Float and center the focused window
        withFocused $ windows . flip XMSS.float xwmMedRect)
    , ((winKey .|. shiftMask .|. controlMask, xK_t),
        -- Float and center the focused window with quite small dimension
        withFocused $ windows . flip XMSS.float xwmSmallRect)
    , ((winKey,                               xK_a),
        -- Spawn the default terminal
        spawn =<< terminalFromConf)
    , ((winKey .|. shiftMask,                 xK_a),
        -- Spawn the secondary terminal
        spawn "st")
    , ((winKey .|. controlMask,               xK_a),
        -- Spawn a tabbed secondary terminal
        spawn "tabbed -c -r 2 st -w ''")
    , ((winKey,                               xK_d),
        -- Spawn emacs
        spawn "emacs")
    , ((winKey,                               xK_f),
        -- Spawn web browser
        spawn "vivaldi-stable")
    , ((winKey .|. shiftMask,                 xK_f),
        -- Spawn secondary web browser
        spawn "surf-open")
    , ((winKey,                               xK_b),
        -- Toggle the status bar gap
        sendMessage ToggleStruts)
    ---------------------------------------------------------------------------
        -- Right side characters

    , ((winKey .|. shiftMask .|. controlMask, xK_y),
        -- Terminal scratchpad
        namedScratchpadAction xwmSPDs "Yakuake")
    , ((winKey,                               xK_u),
        -- Spawn dmenu launcher
        spawn "dmenu_run")
    , ((winKey .|. shiftMask,                 xK_u),
        -- Spawn rofi launcher
        spawn "rofi -modi drun,run,combi -show combi")
    , ((winKey .|. controlMask,               xK_u),
        -- Spawn xmenu launcher
        spawn "xmenu-apps")
    , ((winKey .|. shiftMask .|. controlMask, xK_o),
        -- Emacs-Org-mode scratchpad
        namedScratchpadAction xwmSPDs "Orgenda")
    , ((winKey,                               xK_k),
        -- Move focus to the previous window
        windows XMSS.focusUp)
    , ((winKey,                               xK_j),
        -- Move focus to the next window
        windows XMSS.focusDown)
    , ((winKey .|. shiftMask,                 xK_k),
        -- Swap the focused window with the previous window
        windows XMSS.swapUp)
    , ((winKey .|. shiftMask,                 xK_j),
        -- Swap the focused window with the next window
        windows XMSS.swapDown)
    , ((winKey,                               xK_h),
        -- Shrink the master area
        sendMessage Shrink)
    , ((winKey,                               xK_l),
        -- Expand the master area
        sendMessage Expand)
    , ((winKey .|. shiftMask,                 xK_h),
        -- Shrink area of the focused client in the focused area
        sendMessage MirrorShrink)
    , ((winKey .|. shiftMask,                 xK_l),
        -- Expand area of the focused client in the focused area
        sendMessage MirrorExpand)
    -- Resize viewed windows to the correct size
    -- , ((winKey,               xK_n     ), refresh)
    , ((winKey .|. shiftMask .|. controlMask, xK_n),
        -- Ncmpcpp scratchpad
        namedScratchpadAction xwmSPDs "Ncmpcpp")
    , ((winKey,                               xK_m),
        -- Move focus to the master window
        windows XMSS.focusMaster)
    , ((winKey .|. shiftMask,                 xK_m),
        -- Swap the focused window and the master window
        windows XMSS.swapMaster)
    , ((winKey .|. shiftMask .|. controlMask, xK_m),
        -- Cmus scratchpad
        namedScratchpadAction xwmSPDs "Cmus")
    ---------------------------------------------------------------------------
        -- Surrounding keys
    , ((winKey,                               xK_Tab),
        -- Cycle forward through the available layout algorithms
        sendMessage NextLayout)
    , ((winKey .|. shiftMask,                 xK_Tab),
        --  Reset the layouts on the current workspace to default
        -- setLayout $ XMonad.layoutHook conf)
        setLayout =<< asks (layoutHook . config))
    , ((winKey,                               xK_Delete),
        -- Spawn shutdown menu
        spawn "xmenu-shutdown")
    , ((winKey .|. shiftMask,                 xK_Delete),
        -- Quit xmonad
        io exitSuccess)
    , ((winKey .|. shiftMask,                 xK_BackSpace),
        -- Screenlocker
        spawn "slock")
    , ((winKey,                               xK_Return),
        -- Spawn the default terminal
        spawn =<< terminalFromConf)
    , ((winKey .|. shiftMask,                 xK_Return),
        -- Spawn the secondary terminal
        spawn "st")
    , ((winKey .|. controlMask,               xK_Return),
        -- Spawn a tabbed secondary terminal
        spawn "tabbed -c -r 2 st -w ''")
    , ((winKey,                               xK_comma),
        -- Increment the number of windows in the master area
        sendMessage $ IncMasterN 1)
    , ((winKey,                               xK_period),
        -- Deincrement the number of windows in the master area
        sendMessage $ IncMasterN (-1))
    , ((winKey,                               xK_Print),
        -- "take fullscreen screenshot"
        unGrab >> spawn ("scrot ~/Pictures/Screenshots/%Y-%m-%d-%T-screenshoot.png;" ++
            "notify-send -i photo 'Taken fullscreen screenshot'"))
    , ((winKey .|. shiftMask,                 xK_Print),
        -- "select a rectangular region to take a screenshot"
        unGrab >> spawn ("scrot -s -l style=dash ~/Pictures/Screenshots/$(date +%Y-%m-%d-%T)screenshoot.png;" ++
            "notify-send -i photo 'Saved to ~/Pictures/Screenshots'"))
    ---------------------------------------------------------------------------
        -- Arrow keys
    , ((winKey .|. controlMask,               xK_Left),
        -- float and move to the down-left corner the focused window
        withFocused $ windows . flip XMSS.float xwmDownLeftRect)
    , ((winKey .|. controlMask,               xK_Down),
        -- float and move to the down-right corner the focused window
        withFocused $ windows . flip XMSS.float xwmDownRightRect)
    , ((winKey .|. controlMask,               xK_Right),
        -- float and move to the up-right corner the focused window
        withFocused $ windows . flip XMSS.float xwmUpRightRect)
    , ((winKey .|. controlMask,               xK_Up),
        -- float and move to the up-left corner the focused window
        withFocused $ windows . flip XMSS.float xwmUpLeftRect)
    ---------------------------------------------------------------------------
        -- Fn and XF86 keys
    , ((noModMask,                            XF86.xF86XK_AudioMute),
        -- toggle mute/unmute audio
        spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ((noModMask,                            XF86.xF86XK_AudioLowerVolume),
        -- "decrease volume"
        spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ((noModMask,                            XF86.xF86XK_AudioRaiseVolume),
        -- "increase volume"
        spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ((noModMask,                            XF86.xF86XK_MonBrightnessDown),
        -- "decrease screen brightness"
        spawn "xbacklight -dec 5")
    , ((noModMask,                            XF86.xF86XK_MonBrightnessUp),
        -- "increase screen brightness"
        spawn "xbacklight -inc 5")
    , ((noModMask,                            XF86.xF86XK_Display),
        -- "configure monitor setup"
        spawn "monitor_handler")
    , ((noModMask,                            XF86.xF86XK_Search),
        -- "spawn nnn"
        spawn =<< inTerminalFromConf "nnn")
    , ((noModMask,                            XF86.xF86XK_Explorer),
        -- "spawn file explorer"
        spawn "nautilus")
    , ((noModMask,                            XF86.xF86XK_Calculator),
        -- "spawn calculator"
        spawn =<< inTerminalFromConf "ghci")
    ]
    ++
    -- mod{-Shift}-[1..9], Switch/Move client to workspace N
    [((m .|. winKey, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(XMSS.greedyView, 0), (XMSS.shift, shiftMask)]]
    ++
    -- mod{-Shift}-{w,e,r}, Switch/Move client to physical/Xinerama screens 1, 2, or 3
    [((m .|. winKey, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(XMSS.view, 0), (XMSS.shift, shiftMask)]]

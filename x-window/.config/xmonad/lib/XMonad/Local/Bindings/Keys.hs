module XMonad.Local.Bindings.Keys
    ( xmKeys
    ) where

import qualified Data.Map as Map
    ( Map
    , fromList
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
import qualified XMonad.Actions.Search as XMSearch
    ( promptSearch
    , selectSearch
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
import XMonad.Prompt.Shell
    ( shellPrompt
    )
import XMonad.Prompt.Man
    ( manPrompt
    )

import XMonad.Local.Manage.Util
    ( xmScratchpads
    , xmBigRect
    )
import XMonad.Local.Bindings.Util
    ( xmPromptConfig
    , xmSearchEngineMap
    , xmTreeSelectAction
    , xmTreeSelectConfig
    )

xmKeys :: XConfig Layout -> Map.Map (KeyMask, KeySym) (X ())
xmKeys conf@XConfig {XMonad.modMask = modm} = Map.fromList $
    [ ((modm,                 xK_q), spawn "xmonad --recompile; xmonad --restart")
    , ((modm .|. shiftMask,   xK_q), kill)
    , ((modm,                 xK_t), withFocused $ windows . XMSS.sink)
    , ((modm .|. shiftMask,   xK_t), withFocused $ windows . flip XMSS.float xmBigRect)

    , ((modm,                 xK_a), xmTreeSelectAction xmTreeSelectConfig)

    , ((modm,                 xK_s), incWindowSpacing 1)
    , ((modm .|. shiftMask,   xK_s), decWindowSpacing 1)
    , ((modm .|. controlMask, xK_s), setScreenWindowSpacing 0)

    , ((modm .|. shiftMask,   xK_d), sendMessage $ ToggleStrut D)

    , ((modm,                 xK_f), spawn "vivaldi-stable")
    , ((modm .|. shiftMask,   xK_f), refresh)

    , ((modm,                 xK_b), sendMessage ToggleStruts)

    , ((modm .|. controlMask, xK_m), namedScratchpadAction xmScratchpads "ncmpcpp")
    , ((modm .|. controlMask, xK_y), namedScratchpadAction xmScratchpads "yakuake")
    , ((modm .|. controlMask, xK_o), namedScratchpadAction xmScratchpads "orgenda")

    , ((modm,                 xK_p), XMSM.submap . Map.fromList $
        [ ((0, xK_h),     spawn "mpc prev")
        , ((0, xK_l),     spawn "mpc next")
        , ((0, xK_k),     spawn "mpc play")
        , ((0, xK_k),     spawn "mpc pause")
        , ((0, xK_space), spawn "mpc toggle")
        , ((0, xK_t),     spawn "mpv_bulk_toggle")
        , ((0, xK_q),     spawn "mpv_bulk_quit")
        ])

    , ((modm,                 xK_u), spawn "rofi -modi drun,run -show drun")
    , ((modm .|. shiftMask,   xK_u), spawn "dmenu_run_timed")
    , ((modm .|. controlMask, xK_u), spawn "xmenu-apps")

    , ((modm,                               xK_h), sendMessage Shrink)
    , ((modm,                               xK_l), sendMessage Expand)
    , ((modm,                               xK_j), windows XMSS.focusDown)
    , ((modm,                               xK_k), windows XMSS.focusUp)
    , ((modm .|. shiftMask,                 xK_h), sendMessage MirrorShrink)
    , ((modm .|. shiftMask,                 xK_l), sendMessage MirrorExpand)
    , ((modm .|. shiftMask,                 xK_j), windows XMSS.swapDown)
    , ((modm .|. shiftMask,                 xK_k), windows XMSS.swapUp)
    , ((modm .|. controlMask,               xK_h), sendMessage $ Go L)
    , ((modm .|. controlMask,               xK_l), sendMessage $ Go R)
    , ((modm .|. controlMask,               xK_j), sendMessage $ Go D)
    , ((modm .|. controlMask,               xK_k), sendMessage $ Go U)
    , ((modm .|. mod1Mask,                  xK_h), sendMessage $ Swap L)
    , ((modm .|. mod1Mask,                  xK_l), sendMessage $ Swap R)
    , ((modm .|. mod1Mask,                  xK_j), sendMessage $ Swap D)
    , ((modm .|. mod1Mask,                  xK_k), sendMessage $ Swap U)
    , ((modm .|. controlMask .|. shiftMask, xK_h), sendMessage $ Move L)
    , ((modm .|. controlMask .|. shiftMask, xK_l), sendMessage $ Move R)
    , ((modm .|. controlMask .|. shiftMask, xK_j), sendMessage $ Move D)
    , ((modm .|. controlMask .|. shiftMask, xK_k), sendMessage $ Move U)

    , ((modm,                               xK_m), windows XMSS.focusMaster)
    , ((modm .|. shiftMask,                 xK_m), windows XMSS.swapMaster)

    , ((modm,                 xK_comma    ), sendMessage (IncMasterN 1))
    , ((modm,                 xK_period   ), sendMessage (IncMasterN (-1)))

    , ((modm,                 xK_bracketleft),  prevWS)
    , ((modm,                 xK_bracketright), nextWS)
    , ((modm .|. shiftMask,   xK_bracketleft),  shiftToPrev >> prevWS)
    , ((modm .|. shiftMask,   xK_bracketright), shiftToNext >> nextWS)

    , ((modm,                 xK_Tab      ), sendMessage NextLayout)
    , ((modm .|. shiftMask,   xK_Tab      ), setLayout $ XMonad.layoutHook conf)

    , ((modm,                 xK_space    ), sendMessage $ JumpToLayout "Monocle")
    , ((modm .|. shiftMask,   xK_space    ), sendMessage $ JumpToLayout "Float")

    , ((modm .|. shiftMask,   xK_Delete   ), io exitSuccess)
    , ((modm .|. shiftMask,   xK_BackSpace), spawn "slock")

    , ((modm,                 xK_Return   ), spawn "alacritty")
    , ((modm .|. shiftMask,   xK_Return   ), spawn $ XMonad.terminal conf)

    , ((0,                    xK_Print    ), spawn "scrot ~/Pictures/Screenshots/%Y-%m-%d-%T-screenshoot.png; notify-send -i photo 'Taken fullscreen screenshot'")
    , ((0    .|. shiftMask,   xK_Print    ), spawn "import ~/Pictures/Screenshots/$(date +%Y-%m-%d-%T)screenshoot.png; notify-send -i photo 'Saved to ~/Pictures/Screenshots'")

    , ((modm,               xK_F1 ), manPrompt   xmPromptConfig)
    , ((modm,               xK_F5 ), shellPrompt xmPromptConfig)
    , ((modm,               xK_F2 ), XMSM.submap $ xmSearchEngineMap $ XMSearch.promptSearch xmPromptConfig)
    , ((modm .|. shiftMask, xK_F2 ), XMSM.submap $ xmSearchEngineMap XMSearch.selectSearch)


    , ((0, XF86.xF86XK_AudioMute),         spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ((0, XF86.xF86XK_AudioLowerVolume),  spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ((0, XF86.xF86XK_AudioRaiseVolume),  spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")

    , ((0, XF86.xF86XK_MonBrightnessUp),   spawn "xbacklight -inc 5")
    , ((0, XF86.xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
    , ((0, XF86.xF86XK_Display),           spawn "monitor_handler")
    -- XF86Tools
    -- XF86Search
    -- XF86LaunchA
    -- XF86Explorer
    ]
    ++
    -- Switch/MoveClient to workspace N --> mod(+Shift)-[1..9]
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(XMSS.greedyView, 0), (XMSS.shift, shiftMask)]]
    ++
    -- Switch/MoveClient to physical/Xinerama screens 1, 2, or 3 --> mod(+Shift)-{w,e,r}
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(XMSS.view, 0), (XMSS.shift, shiftMask)]]

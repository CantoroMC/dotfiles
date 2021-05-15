import XMonad

-- import XMonad.Util.Ungrab
-- import XMonad.Hooks.ManageDocks
-- import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers
import XMonad.Hooks.ManageDocks (docks)
-- import XMonad.Util.ClickableWorkspaces

import Bindings.Keys (xwmKeys)
import Layout.Hook (xmLayoutHook)
import Manage.Hook (xmManageHook)

------------------------------------------------------------------------
-- | Mouse bindings: default actions bound to mouse events
--
-- myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
--     -- mod-button1, Set the window to floating mode and move by dragging
--     [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)

--     -- mod-button2, Raise the window to the top of the stack
--     , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

--     -- mod-button3, Set the window to floating mode and resize by dragging
--     , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)

--     -- you may also bind events to the mouse scroll wheel (button4 and button5)
--     ]
------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
-- myStartupHook = return ()

xbarPP :: PP
xbarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }  where
    formatFocused   = wrap (white "[") (white "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow =
        xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

-- | Xwm Configuration
xmConfig = def {
    terminal           = "kitty"
    , focusFollowsMouse  = False
    , clickJustFocuses   = True
    , borderWidth        = 1
    , modMask            = mod4Mask
    , workspaces         = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    , normalBorderColor  = "#151a1e"
    , focusedBorderColor = "#b8cc52"

    , keys               = xwmKeys
    -- , mouseBindings      = xmMouseBindings

    , manageHook         = xmManageHook
    -- , handleEventHook    = myEventHook
    -- , logHook            = xmLogHook xmproc
    -- , startupHook        = xmStartupHook
    , layoutHook         = xmLayoutHook
    }

main :: IO ()
main =
    xmonad
        . docks
        . ewmh
        . withEasySB (statusBarProp "xbar" (pure xbarPP)) defToggleStrutsKey
        $ xmConfig

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

import Bindings.Keys (xmKeys)
import Layout.Hook (xmLayoutHook)
import Manage.Hook (xmManageHook)


xmXmobarPP :: PP
xmXmobarPP = def
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

xmConfig = def
    { terminal           = "kitty"
    , focusFollowsMouse  = False
    , clickJustFocuses   = True
    , borderWidth        = 1
    , modMask            = mod4Mask
    , workspaces         = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    , normalBorderColor  = "#151a1e"
    , focusedBorderColor = "#b8cc52"
    -- More complicated stuff
    , keys               = xmKeys
    , manageHook         = xmManageHook
    , layoutHook         = xmLayoutHook
    }

main :: IO ()
main =
    xmonad
        . docks
        . ewmh
        . withEasySB (statusBarProp "xbar" (pure xmXmobarPP)) defToggleStrutsKey
        $ xmConfig


  -- [ ("M-S-z", spawn "xscreensaver-command -lock")
  --     , ("M-S-=", unGrab *> spawn "scrot -s"        )
  --     , ("M-]"  , spawn "firefox"                   )
  -- ]

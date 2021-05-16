import XMonad
    ( XConfig(
        terminal
        , focusFollowsMouse
        , clickJustFocuses
        , borderWidth
        , modMask
        , workspaces
        , normalBorderColor
        , focusedBorderColor
        , keys
        , manageHook
        , startupHook
        , layoutHook
        )
    , Default(def)
    , mod4Mask
    , xmonad
    )
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (docks)
import XMonad.Hooks.StatusBar (withSB)

import Bindings.Keys (xwmKeys)
import qualified Config.Theme as XwmTheme
import Config.Workspaces (xwmWorkspaces)
import Layout.Hook (xwmLayoutHook)
import Manage.Hook (xwmManageHook)
import Log.StatusBar (xBarConfig)
import Startup.Hook (xwmStartupHook)



main :: IO ()
main = do
    let xwmConfig' = def {
        terminal             = "kitty"
        , focusFollowsMouse  = False
        , clickJustFocuses   = True
        , borderWidth        = XwmTheme.borderWidth XwmTheme.xwmTheme
        , modMask            = mod4Mask
        , workspaces         = xwmWorkspaces
        , normalBorderColor  = XwmTheme.inactiveBorderColor XwmTheme.xwmTheme
        , focusedBorderColor = XwmTheme.activeBorderColor   XwmTheme.xwmTheme

        , keys               = xwmKeys
        -- , mouseBindings      = xmMouseBindings

        , manageHook         = xwmManageHook
        -- , handleEventHook    = myEventHook
        -- , logHook            = xmLogHook xmproc
        , startupHook        = xwmStartupHook
        , layoutHook         = xwmLayoutHook
        }
        xwmConfig = docks . ewmh . withSB xBarConfig $ xwmConfig'
    xmonad xwmConfig

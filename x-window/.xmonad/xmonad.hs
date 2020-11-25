import XMonad
import XMonad.Hooks.ManageDocks
    ( docks
    )
import XMonad.Hooks.EwmhDesktops
    ( ewmh
    )

import qualified XMonad.Local.Config.Theme as XMTheme
import XMonad.Local.Config.Workspace
    ( workspaceIds
    )

import XMonad.Local.Log.Hook
    ( xmLogHook
    )
import XMonad.Local.Log.XMobar
    ( spawnXMobar
    )

main :: IO ()
main = do
    xmproc <- spawnXMobar
    xmonad $ docks $ ewmh def
        { terminal           = "alacritty"
        , modMask            = mod4Mask
        , focusFollowsMouse  = False
        , clickJustFocuses   = True
        , workspaces         = workspaceIds
        , borderWidth        = XMTheme.borderWidth XMTheme.xmTheme
        , normalBorderColor  = XMTheme.inactiveBorderColor XMTheme.xmTheme
        , focusedBorderColor = XMTheme.activeBorderColor XMTheme.xmTheme
        -- , keys               = _keys
        -- , mouseBindings      = _mouseBindings
        -- , manageHook         = _manageHook <+> manageDocks
        -- , handleEventHook    = _handleEventHook <+> ewmhDesktopsEventHook
        , logHook            = xmLogHook xmproc
        -- , startupHook        = _startupHook
        -- , layoutHook         = _layoutHook
        }

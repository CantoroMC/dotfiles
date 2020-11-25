------------------------------------------------------------------------------
    -- XMonad and XMonad-Contrib
import XMonad
import XMonad.Hooks.ManageDocks
    ( docks
    )
import XMonad.Hooks.EwmhDesktops
    ( ewmh
    )

------------------------------------------------------------------------------
    -- Local Configuration

import qualified XMonad.Local.Config.Theme as XMTheme
import XMonad.Local.Config.Workspace
    ( xmWorkspaces
    )

import XMonad.Local.Log.Hook
    ( xmLogHook
    )
import XMonad.Local.Log.XMobar
    ( spawnXMobar
    )

import XMonad.Local.Manage.Hook
    ( xmManageHook
    )

------------------------------------------------------------------------------
    -- XMonad Main

main :: IO ()
main = do
    xmproc <- spawnXMobar
    xmonad $ docks $ ewmh def
        { terminal           = "alacritty"
        , modMask            = mod4Mask
        , focusFollowsMouse  = False
        , clickJustFocuses   = True
        , workspaces         = xmWorkspaces
        , borderWidth        = XMTheme.borderWidth XMTheme.xmTheme
        , normalBorderColor  = XMTheme.inactiveBorderColor XMTheme.xmTheme
        , focusedBorderColor = XMTheme.activeBorderColor XMTheme.xmTheme
        -- , keys               = _keys
        -- , mouseBindings      = _mouseBindings
        , manageHook         = xmManageHook
        , logHook            = xmLogHook xmproc
        -- , startupHook        = _startupHook
        -- , layoutHook         = _layoutHook
        }

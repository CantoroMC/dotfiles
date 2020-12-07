------------------------------------------------------------------------------
    -- XMonad and XMonad-Contrib
import XMonad
import XMonad.Actions.UpdatePointer
    ( updatePointer
    )
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

import XMonad.Local.Startup.Hook
    ( xmStartupHook
    )

import XMonad.Local.Urgency.Hook
    ( applyUrgencyHook
    )

import XMonad.Local.Layout.Hook
    ( xmLayoutHook
    )

import XMonad.Local.Bindings.Keys
    ( xmKeys
    )
import XMonad.Local.Bindings.Mouse
    ( xmMouseBindings
    )

------------------------------------------------------------------------------
    -- XMonad Main

main :: IO ()
main = do
    xmproc <- spawnXMobar
    let c = def
            { terminal           = "st"
            , modMask            = mod4Mask
            , focusFollowsMouse  = False
            , clickJustFocuses   = True
            , workspaces         = xmWorkspaces
            , borderWidth        = XMTheme.borderWidth XMTheme.xmTheme
            , normalBorderColor  = XMTheme.inactiveBorderColor XMTheme.xmTheme
            , focusedBorderColor = XMTheme.activeBorderColor XMTheme.xmTheme
            , keys               = xmKeys
            , mouseBindings      = xmMouseBindings
            , manageHook         = xmManageHook
            , logHook            = xmLogHook xmproc >> updatePointer (0.5, 0.5) (0, 0)
            , startupHook        = xmStartupHook
            , layoutHook         = xmLayoutHook
            }
        xmConf = docks . applyUrgencyHook . ewmh $ c
    xmonad xmConf

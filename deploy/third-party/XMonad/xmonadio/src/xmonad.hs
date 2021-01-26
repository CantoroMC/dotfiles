import           XMonad
import           XMonad.Hooks.EwmhDesktops      ( ewmh )
import           XMonad.Hooks.ManageDocks       ( docks )

import qualified Config.Theme     as XMTheme
import           Config.Workspace  ( xmWorkspaces )

import           Log.Hook          ( xmLogHook )
import           Log.XMobar        ( spawnXMobar )

import           Manage.Hook       ( xmManageHook )

import           Startup.Hook      ( xmStartupHook )

import           Urgency.Hook      ( applyUrgencyHook )

import           Layout.Hook       ( xmLayoutHook )

import           Bindings.Bind     ( mapBindings
                                   , storeBindings
                                   )

import           Bindings.Keys     ( xmKeys )
import           Bindings.Mouse    ( xmMouseBindings )

main :: IO ()
main = do
  xmproc <- spawnXMobar
  let (applicableKeys, explainableBindings) = mapBindings $ xmKeys . modMask
      c = def { terminal           = "st"
              , modMask            = mod4Mask
              , focusFollowsMouse  = False
              , clickJustFocuses   = True
              , workspaces         = xmWorkspaces
              , borderWidth        = XMTheme.borderWidth XMTheme.xmTheme
              , normalBorderColor  = XMTheme.inactiveBorderColor XMTheme.xmTheme
              , focusedBorderColor = XMTheme.activeBorderColor XMTheme.xmTheme
              , keys               = applicableKeys
              , mouseBindings      = xmMouseBindings
              , manageHook         = xmManageHook
              , logHook            = xmLogHook xmproc
              , startupHook        = xmStartupHook
              , layoutHook         = xmLayoutHook
              }
      xmConf =
        storeBindings explainableBindings
          . docks
          . applyUrgencyHook
          . ewmh
          $ c
  xmonad xmConf

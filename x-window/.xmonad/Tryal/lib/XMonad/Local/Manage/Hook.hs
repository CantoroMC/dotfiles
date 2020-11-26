module XMonad.Local.Manage.Hook
    ( xmManageHook
    ) where

import Control.Monad 
    ( liftM2
    )

import XMonad
import qualified XMonad.StackSet as XMSS

import XMonad.Actions.SpawnOn
    ( manageSpawn -- hook for spawnOn and spawnHere
    )
import XMonad.Hooks.ManageDocks
    ( manageDocks
    )
import XMonad.Hooks.ManageHelpers
    ( doCenterFloat
    , doRectFloat
    )
import XMonad.Util.NamedScratchpad
    ( namedScratchpadManageHook
    )

import XMonad.Local.Manage.Util
    ( xmScratchpads
    , xmBigRect
    )
import XMonad.Local.Config.Workspace
    ( xmWorkspaces
    )

xmManageHook :: ManageHook
xmManageHook =
    manageSpawn <+> manageDocks <+>
    manageScratchpads <+> manageFloatings <+> manageOthers

manageScratchpads :: ManageHook
manageScratchpads = namedScratchpadManageHook xmScratchpads

manageOthers :: ManageHook
manageOthers = composeAll
    [ -- doShift
      className =? "Transmission-gtk" --> doShift      (xmWorkspaces !! 8)
    , className =? "mpv"              --> doShiftAndGo (xmWorkspaces !! 4)
      -- doIgnore
    , resource  =? "stalonetray"      --> doIgnore
    ] where doShiftAndGo = doF . liftM2 (.) XMSS.greedyView XMSS.shift

manageFloatings :: ManageHook
manageFloatings = composeAll $
    [ className =? appToFloat --> doCenterFloat | appToFloat <- appsToFloat ]
    ++
    [ title =? "Event Tester"  --> doFloat
    , title =? "lstopo"        --> doCenterFloat
    , title =? "weatherreport" --> doRectFloat xmBigRect
    , role  =? "ncmpcpp"       --> doCenterFloat
    , role  =? "pop-up"        --> doCenterFloat
    , (className =? "Display" <&&> title =? "ImageMagick: ")                       --> doCenterFloat
    , (className =? "Thunar"  <&&> title =? "Bulk Rename - Rename Multiple Files") --> doCenterFloat
    , (className =? "Thunar"  <&&> title =? "File Operation Progress")             --> doCenterFloat
    ] where appsToFloat = [ "Arandr"
                          , "Avahi-discover"
                          , "Baobab"
                          , "Blueberry.py"
                          , "Bssh"
                          , "Bvnc"
                          , "CMakeSetup"
                          , "Exo-helper-2"
                          , "feh"
                          , "Gimp"
                          , "Gnome-disks"
                          , "Gpick"
                          , "Hardinfo"
                          , "imagewriter"
                          , "Lightdm-gtk-greeter-settings"
                          , "Lxappearance"
                          , "MPlayer"
                          , "Nitrogen"
                          , "Pavucontrol"
                          , "qv4l2"
                          , "qvidcap"
                          , "Sxiv"
                          , "System-config-printer.py"
                          , "Transmission-gtk"
                          , "Xarchiver"
                          , "Xboard"
                          , "Xfce4-about"
                          , "Xmessage"
                          , "Yad"
                          , "Yad-icon-browser"
                          ]

role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

-- name :: Query String
-- name = stringProperty "WM_NAME"

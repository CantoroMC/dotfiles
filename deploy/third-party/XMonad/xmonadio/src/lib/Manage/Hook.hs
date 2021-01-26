{-# LANGUAGE FlexibleContexts #-}

module Manage.Hook
    ( xmManageHook
    ) where

import Control.Monad
    ( liftM2
    )

import Data.Maybe ( isNothing )
import Text.Regex ( matchRegex, mkRegex )

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

import Manage.Util
    ( xmScratchpads
    , xmBigRect
    , xmMedRect
    )
import Config.Workspace
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
      className =? "Transmission-gtk"             --> doShift      (xmWorkspaces !! 8)
    , className =? "mpv"                          --> doShiftAndGo (xmWorkspaces !! 4)
    , className =? "MATLAB R2019b - academic use" --> doShiftAndGo (xmWorkspaces !! 1)
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
    , title =? "keysheet"      --> doRectFloat xmBigRect
    , title =? "calendar"      --> doRectFloat xmBigRect
    , title =? "volume"        --> doRectFloat xmMedRect
    , role  =? "cmus"          --> doCenterFloat
    , role  =? "pop-up"        --> doCenterFloat
    , (className =? "Display"                      <&&> title =? "ImageMagick: ") --> doCenterFloat
    -- , (className =? "MATLAB R2019b - academic use" <&&> title =? "Help")          --> doRectFloat xmBigRect
    -- , (className =? "MATLAB R2019b - academic use" <&&> title =? "Preferences")   --> doRectFloat xmBigRect
    , (className =? "MATLAB R2019b - academic use" <&&> title *!? "^MATLAB") --> doRectFloat xmBigRect
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
                          , "Lxappearance"
                          , "MPlayer"
                          , "Nitrogen"
                          , "ParaView"
                          , "Parcellite"
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

(*!=) :: String -> String -> Bool
q *!= x = isNothing $ matchRegex (mkRegex x) q

(*!?) :: Functor f => f String -> String -> f Bool
q *!? x = fmap (*!= x) q

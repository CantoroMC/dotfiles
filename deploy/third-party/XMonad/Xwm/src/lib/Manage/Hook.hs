{-# LANGUAGE FlexibleContexts #-}

module Manage.Hook
    ( xmManageHook
    ) where



import Control.Monad (liftM2)

import Data.Maybe (isNothing)
import Text.Regex (matchRegex, mkRegex)

import XMonad
import qualified XMonad.StackSet as XMSS

-- import XMonad.Actions.SpawnOn (manageSpawn)
-- import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.ManageHelpers (doCenterFloat, doRectFloat)

import Manage.Util (xwmBigRect, xwmMedRect)


-- workspacesFromConf :: (MonadIO m, MonadReader XConf m) => m String
-- workspacesFromConf = reader $ workspaces . config


role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

-- name :: Query String
-- name = stringProperty "WM_NAME"

(*!=) :: String -> String -> Bool
q *!= x = isNothing $ matchRegex (mkRegex x) q

(*!?) :: Functor f => f String -> String -> f Bool
q *!? x = fmap (*!= x) q


manageOthers :: ManageHook
manageOthers = composeAll
    [ -- doShift
      -- className =? "Transmission-gtk" --> doShift (xmWorkspaces !! 8)
    -- , className =? "mpv" --> doShiftAndGo (xmWorkspaces !! 4)
    -- , className =? "MATLAB R2021a - academic use" --> doShiftAndGo (xmWorkspaces !! 1)
    -- doIgnore
    resource  =? "stalonetray" --> doIgnore
    , className =? "Conky" --> doIgnore
    ]
    -- where doShiftAndGo = doF . liftM2 (.) XMSS.greedyView XMSS.shift

manageFloatings :: ManageHook
manageFloatings =
    composeAll
        $  [ className =? appToFloat --> doCenterFloat
           | appToFloat <- appsToFloat
           ]
        ++ [ title =? "Event Tester" --> doFloat
           , title =? "lstopo" --> doCenterFloat
           , title =? "weatherreport" --> doRectFloat xwmBigRect
           , title =? "keysheet" --> doRectFloat xwmBigRect
           , title =? "orgenda" --> doRectFloat xwmBigRect
           , title =? "volume" --> doRectFloat xwmMedRect
           , role =? "pop-up" --> doCenterFloat
           , (className =? "Display" <&&> title =? "ImageMagick: ") --> doCenterFloat
           , (className =? "MATLAB R2021a - academic use" <&&> title *!?  "^MATLAB") --> doFloat
           ]  where
    appsToFloat =
        [ "Arandr"
        , "Avahi-discover"
        , "Baobab"
        , "Blueberry.py"
        , "Bssh"
        , "Bvnc"
        , "CMakeSetup"
        , "Exo-helper-2"
        , "feh"
        , "File-roller"
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
        , "Snapper-gui"
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

xmManageHook :: ManageHook
xmManageHook = manageFloatings <+> manageOthers
-- xmManageHook = manageSpawn <+> manageDocks <+> manageFloatings <+> manageOthers


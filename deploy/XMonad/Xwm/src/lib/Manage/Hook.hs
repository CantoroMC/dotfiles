{-# LANGUAGE FlexibleContexts #-}

module Manage.Hook
    ( xwmManageHook
    ) where



import Control.Monad (liftM2)

import Data.Maybe (isNothing)
import Text.Regex (matchRegex, mkRegex)

import XMonad
import qualified XMonad.StackSet as XMSS

import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.ManageHelpers
    ( doCenterFloat
    , doRectFloat
    , (-?>)
    , isDialog
    , transience
    , composeOne
    )
import XMonad.Util.NamedScratchpad (namedScratchpadManageHook)

import Config.Workspaces (xwmWorkspaces)
import Manage.Util (xwmBigRect, xwmMedRect, xwmSideLeft, xwmSPDs)



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
      className =? "Transmission-gtk" --> doShift (xwmWorkspaces !! 8)
    , className =? "mpv" --> doShiftAndGo (xwmWorkspaces !! 4)
    , className =? "MATLAB R2021a - academic use" --> doShiftAndGo (xwmWorkspaces !! 1)
    -- doIgnore
    , resource  =? "stalonetray" --> doIgnore
    , className =? "Conky" --> doIgnore
    ]
    where doShiftAndGo = doF . liftM2 (.) XMSS.greedyView XMSS.shift

manageFloatings :: ManageHook
manageFloatings =
    composeAll $
        [ className =? appToFloat --> doCenterFloat
           | appToFloat <- appsToFloat
        ]
        ++
        [ title =? "Event Tester" --> doFloat
        , title =? "lstopo" --> doCenterFloat
        , title =? "weatherreport" --> doRectFloat xwmBigRect
        , title =? "keysheet" --> doRectFloat xwmSideLeft
        , title =? "volume" --> doRectFloat xwmMedRect
        , role  =? "pop-up" --> doCenterFloat
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
                , "Gnome-disks"
                , "feh"
                , "Hardinfo"
                , "imagewriter"
                , "Lxappearance"
                , "MPlayer"
                , "Nibbler"
                , "Nm-connection-editor"
                , "ParaView"
                , "Parcellite"
                , "Pavucontrol"
                , "qv4l2"
                , "qvidcap"
                , "Snapper-gui"
                , "Sxiv"
                , "System-config-printer.py"
                , "Transmission-gtk"
                , "Xboard"
                , "Xmessage"
                , "Yad"
                , "Yad-icon-browser"
                ]

xwmManageHook :: ManageHook
xwmManageHook =
    composeOne
        [ transience
        , isDialog -?> doCenterFloat
        ]
    <+> namedScratchpadManageHook xwmSPDs
    <+> manageDocks
    <+> manageFloatings
    <+> manageOthers

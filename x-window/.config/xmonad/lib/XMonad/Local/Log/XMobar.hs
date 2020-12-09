module XMonad.Local.Log.XMobar
    ( xmXMobarPP
    , spawnXMobar
    ) where

import GHC.IO.Handle
    ( Handle
    )

import XMonad
import XMonad.Hooks.DynamicLog
    ( PP (..)
    , xmobarPP
    , xmobarColor
    , shorten
    )
import XMonad.Util.Run
    ( hPutStrLn
    , spawnPipe
    )

import qualified XMonad.Local.Config.Theme as XMTheme
import XMonad.Local.Config.Workspace
    ( Workspace
    )

spawnXMobar :: MonadIO m => m Handle
spawnXMobar = spawnPipe $ unwords
    [ executable
    , flagIconroot
    -- , fileXMobarRc
    ] where executable       = xMobarConfigHome <> "/xmobar"
            flagIconroot     = "--iconroot=" <> xMobarConfigHome <> "/icons/" <> XMTheme.xmBackGround
            -- fileXMobarRc     = xMobarConfigHome <> "/xmobarrc"
            xMobarConfigHome = "\"${XDG_CONFIG_HOME}\"/xmobar"

xmXMobarPP :: Handle -> PP
xmXMobarPP h = xmobarPP
    { ppCurrent         = clickableIcon "current"
    , ppVisible         = clickableIcon "visible"
    , ppUrgent          = clickableIcon "urgent"
    , ppHidden          = clickableIcon "hidden"
    , ppHiddenNoWindows = clickableIcon "hiddenNoWindows"
    , ppSep             = " "
    , ppWsSep           = ""
    , ppTitle           = xmobarColor (XMTheme.activeTextColor XMTheme.xmTheme) "" . shorten 30
    , ppLayout          = xmobarColor (XMTheme.urgentBorderColor XMTheme.xmTheme) ""
    , ppOutput          = hPutStrLn h
    -- , ppOrder           = \(ws:l:t) -> [ws,l] ++ t
    }

clickableIcon :: String -> WorkspaceId -> String
clickableIcon status wsId = let ws = read wsId :: Workspace
                                n = show $ 1 + fromEnum ws
                            in "<action=`xdotool key 0xffeb+"        <> n <> "` button=1>" <>
                               "<action=`xdotool key 0xffeb+0xffe1+" <> n <> "` button=3>" <>
                               "<icon=workspaces/" <> status <> "/workspace_" <> n <> ".xpm/>" <>
                               "</action></action>"

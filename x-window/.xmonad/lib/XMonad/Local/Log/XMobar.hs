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
    , wrap
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
    , fileXMobarRc
    ] where executable       = "xmobar"
            flagIconroot     = "--iconroot=" <> xMobarConfigHome <> "/icons"
            fileXMobarRc     = xMobarConfigHome <> "/xmobarrc"
            xMobarConfigHome = "\"${XDG_CONFIG_HOME}\"/xmobar"

xmXMobarPP :: Handle -> PP
xmXMobarPP h = xmobarPP
    { ppOutput          = hPutStrLn h
    , ppOrder           = \ (wss:_) -> [wss]
    , ppWsSep           = ""
    , ppCurrent         = clickableIcon "current"
    , ppVisible         = clickableIcon "visible"
    , ppUrgent          = clickableIcon "urgent"
    , ppHidden          = clickableIcon "hidden"
    , ppHiddenNoWindows = clickableIcon "hiddenNoWindows"
    }

clickableIcon :: String -> WorkspaceId -> String
clickableIcon status wsId = let ws = read wsId :: Workspace
                                n = show $ 1 + fromEnum ws
                            in "<action=xdotool key super+" <> n <> ">" <>
                               "<icon=workspaces/" <> status <> "/workspace_" <> n <> ".xpm/>" <>
                               "</action>"

module XMonad.Local.Log.XMobar
    ( xmXMobarPP
    , spawnXMobar
    ) where

import           GHC.IO.Handle                  ( Handle )

import           XMonad
import qualified XMonad.StackSet               as XMSS

import           XMonad.Hooks.DynamicLog        ( PP(..)
                                                , shorten
                                                , xmobarColor
                                                , xmobarPP
                                                )
import           XMonad.Util.Run                ( hPutStrLn
                                                , spawnPipe
                                                )

import qualified XMonad.Local.Config.Theme     as XMTheme
import           XMonad.Local.Config.Workspace  ( Workspace )

spawnXMobar :: MonadIO m => m Handle
spawnXMobar = spawnPipe $ unwords [executable, flagIconroot]  where
    executable = xMobarConfigHome <> "/xmobarb"
    flagIconroot =
        "--iconroot=" <> xMobarConfigHome <> "/icons/" <> XMTheme.xmBackGround
    xMobarConfigHome = "\"${DOTFILES}\"/deploy/third-party/XMonad/xmobarbarian"

xmXMobarPP :: Handle -> PP
xmXMobarPP h = xmobarPP
    { ppCurrent         = clickableIcon "current"
    , ppVisible         = clickableIcon "visible"
    , ppUrgent          = clickableIcon "urgent"
    , ppHidden          = clickableIcon "hidden"
    , ppHiddenNoWindows = clickableIcon "hiddenNoWindows"
    , ppSep             = " "
    , ppWsSep           = ""
    , ppTitle = xmobarColor (XMTheme.activeTextColor XMTheme.xmTheme) ""
                    . shorten 50
    , ppLayout = xmobarColor (XMTheme.urgentBorderColor XMTheme.xmTheme) ""
    , ppOutput          = hPutStrLn h
    , ppExtras          = [windowCount]
    , ppOrder           = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
    }

clickableIcon :: String -> WorkspaceId -> String
clickableIcon status wsId =
    let ws = read wsId :: Workspace
        n  = show $ 1 + fromEnum ws
    in  "<action=`xdotool key 0xffeb+"
            <> n
            <> "` button=1>"
            <> "<action=`xdotool key 0xffeb+0xffe1+"
            <> n
            <> "` button=3>"
            <> "<icon=workspaces/"
            <> status
            <> "/workspace_"
            <> n
            <> ".xpm/>"
            <> "</action></action>"

windowCount :: X (Maybe String)
windowCount =
    gets
        $ Just
        . show
        . length
        . XMSS.integrate'
        . XMSS.stack
        . XMSS.workspace
        . XMSS.current
        . windowset

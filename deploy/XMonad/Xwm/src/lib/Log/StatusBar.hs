module Log.StatusBar (xBarConfig) where



import XMonad
import qualified XMonad.StackSet as XMSS
import XMonad.Hooks.StatusBar ( statusBarProp, StatusBarConfig )
import XMonad.Hooks.StatusBar.PP
    ( PP( ppCurrent
        , ppVisible
        , ppHidden
        , ppHiddenNoWindows
        , ppVisibleNoWindows
        , ppUrgent
        , ppRename
        , ppSep
        , ppWsSep
        , ppTitle
        , ppTitleSanitize
        , ppLayout
        , ppOrder
        , ppSort
        , ppExtras
        , ppOutput
        )
    , pad
    , shorten
    , wrap
    , xmobarBorder
    , xmobarColor
    , xmobarRaw
    , xmobarAction
    , xmobarStrip
    , filterOutWsPP
    )
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)
import XMonad.Util.WorkspaceCompare (getSortByIndex)

import qualified Config.Theme as XwmTheme
import Log.ClickableWorkspaces (clickablePP)

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

xmobarFont ::
    Int
    -- ^ Additional font index
    -> String
    -- ^ Displayed/wrapped text
    -> String
xmobarFont n  = wrap ("<fn=" ++ show n ++ ">") "</fn>"

xBarPP :: PP
xBarPP = def
    { ppCurrent =
        xmobarColor (XwmTheme.activeTextColor XwmTheme.xwmTheme) ""
        . xmobarBorder "Top" (XwmTheme.activeTextColor XwmTheme.xwmTheme) 2
        . pad
        . xmobarFont 3
    , ppVisible =
        xmobarColor (XwmTheme.inactiveTextColor XwmTheme.xwmTheme) ""
        . xmobarBorder "Top" (XwmTheme.inactiveTextColor XwmTheme.xwmTheme) 1
        . pad
        . xmobarFont 3
    , ppHidden =
        pad
        . xmobarBorder "Bottom" (XwmTheme.urgentTextColor XwmTheme.xwmTheme) 2
        . xmobarFont 3
    , ppHiddenNoWindows  =
        xmobarColor (XwmTheme.activeColor XwmTheme.xwmTheme) ""
        . pad
        . xmobarFont 3
    , ppVisibleNoWindows = Nothing
    , ppUrgent =
        wrap "(" ")"
        . xmobarColor (XwmTheme.urgentColor XwmTheme.xwmTheme) ""
        . pad
        . xmobarFont 3
    , ppRename           = pure
    , ppSep              = " "
    , ppWsSep            = ""
    , ppTitle            = xmobarFont 2 . shorten 50
    , ppTitleSanitize    = xmobarStrip
    , ppLayout           =
        xmobarAction "xdotool key 0xffeb+0x20" "1"
        . xmobarAction "xdotool key 0xffeb+0xffe1+0x20" "2"
        . xmobarAction "xdotool key 0xffeb+0xffe3+0x20" "3"
        . xmobarAction "xdotool key 0xffeb+0xff09" "5"
        . xmobarColor (XwmTheme.inactiveTextColor XwmTheme.xwmTheme) ""
    , ppOrder            = \[ws, l, t, ex] -> [ws, l, ex, t]
    , ppSort             = getSortByIndex
    , ppExtras           = [windowCount]
    , ppOutput           = putStrLn
    }

xBarConfig :: StatusBarConfig
xBarConfig = statusBarProp xbarCmd $ clickablePP $ filterOutWsPP [scratchpadWorkspaceTag] xBarPP
  where
    xbarCmd = unwords ["xbar", flagIconRoot]
    flagIconRoot = "--iconroot=" <> xbarConfigDir <> "/utilities/icons"
    xbarConfigDir = "\"${XMOBAR_CONFIG_DIR}\""

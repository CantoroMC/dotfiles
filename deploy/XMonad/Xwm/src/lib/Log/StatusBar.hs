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
    , xmobarStrip
    , filterOutWsPP
    )
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)
import XMonad.Util.WorkspaceCompare (getSortByIndex)

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
    { ppCurrent          = xmobarColor "#b8cc52" "" . xmobarBorder "Top" "#b8cc52" 2 . pad . xmobarFont 3
    , ppVisible          = xmobarColor "#68d5ff" "" . xmobarBorder "Top" "#68d5ff" 1 . pad . xmobarFont 3
    , ppHidden           = pad . xmobarBorder "Bottom" "#fafafa" 2 . xmobarFont 3
    , ppHiddenNoWindows  = xmobarColor "#3f4e5a" "" . pad . xmobarFont 3
    , ppVisibleNoWindows = Nothing
    , ppUrgent           = wrap "(" ")" . xmobarColor "#ff3333" "" . pad . xmobarFont 3
    , ppRename           = pure
    , ppSep              = " "
    , ppWsSep            = ""
    , ppTitle            = xmobarFont 2 . shorten 50
    , ppTitleSanitize    = xmobarStrip
    , ppLayout           = xmobarColor "#36a3d9" ""
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

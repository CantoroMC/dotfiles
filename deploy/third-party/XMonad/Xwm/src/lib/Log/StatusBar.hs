module Log.StatusBar (xBarConfig) where



import XMonad
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
import XMonad.Util.Loggers ( logTitles )
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)
import XMonad.Util.WorkspaceCompare (getSortByIndex)



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
    , ppUrgent           = wrap " !" " " . xmobarColor "#ff3333" ""
    , ppRename           = pure
    , ppSep              = " "
    , ppWsSep            = ""
    , ppTitle            = shorten 50
    , ppTitleSanitize    = xmobarStrip
    , ppLayout           = xmobarColor "#36a3d9" ""
    , ppOrder            = \[ws, l, _, ex] -> [ws, l, ex]
    , ppSort             = getSortByIndex
    , ppExtras           = [logTitles fmtFocused fmtUnfocused]
    , ppOutput           = putStrLn
    }
      where
        fmtFocused   = xmobarColor "#b8cc52" "" . xmobarFont 1 . wrap "•" "" . ppTitle
        fmtUnfocused = xmobarFont 1 . wrap "•" "" . ppTitle

        ppTitle :: String -> String
        ppTitle = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

xBarConfig :: StatusBarConfig
xBarConfig = statusBarProp xbarCmd $ pure $ filterOutWsPP [scratchpadWorkspaceTag] xBarPP
  where
    xbarCmd = unwords ["xbar", flagIconRoot]
    flagIconRoot = "--iconroot=" <> xbarConfigDir <> "/utilities/icons/dark"
    xbarConfigDir = "\"${XMOBAR_CONFIG_DIR}\""

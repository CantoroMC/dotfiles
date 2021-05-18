module Config (baseConfig) where



import Xmobar
    ( defaultConfig,
      Align(C),
      Border(NoBorder),
      Config
        ( font
        , additionalFonts
        , bgColor
        , fgColor
        , alpha
        , position
        , border
        , borderColor
        , textOffset
        , textOffsets
        , iconRoot
        , iconOffset
        , lowerOnStart
        , pickBroadest
        , persistent
        , hideOnStart
        , allDesktops
        , overrideRedirect
        , sepChar
        , alignSep
        ),
      XPosition(TopSize)
    )

import Colors (Palette(..))



baseConfig :: Palette -> Config
baseConfig p = defaultConfig
    { font             = pFont p
    , additionalFonts  =
        [ "xft:FantasqueSansMono Nerd Font:style=Italic:size=7:hinting=true" -- for the win titles
        , "xft:Operator Mono Lig:style:Italic:size=7:antialias=true:hinting=true"
        , "xft:FiraCode Nerd Font:style=Regular:pixelsize=11" -- for the workspaces
        , "xft:FiraCode Nerd Font:style=Regular:pixelsize=16"
        ]
    , bgColor          = pBackground p
    , fgColor          = pForeground p
    , alpha            = pAlpha p
    , position         = TopSize C 100 24
    , border           = NoBorder
    , borderColor      = pBorder p
    , textOffset       = -1
    , textOffsets      = [-1, -1, -1, -1]
    , iconRoot         = pIconRoot p
    , iconOffset       = -1
    , lowerOnStart     = True
    , pickBroadest     = False
    , persistent       = False
    , hideOnStart      = False
    , allDesktops      = True
    , overrideRedirect = True
    , sepChar          = "|"
    , alignSep         = "}{"
    }

module Config.Theme
    ( Theme(..)
    , xwmTheme
    ) where

import XMonad (Dimension)

import Config.Colors (Color, Colors(..), xwmColors)

data Theme = Theme
    { borderWidth         :: Dimension
    , activeColor         :: Color
    , inactiveColor       :: Color
    , urgentColor         :: Color
    , activeBorderColor   :: Color
    , inactiveBorderColor :: Color
    , urgentBorderColor   :: Color
    , activeTextColor     :: Color
    , inactiveTextColor   :: Color
    , urgentTextColor     :: Color
    }

xwmTheme :: Theme
xwmTheme = Theme
    { borderWidth         = 3
    , activeColor         = bBlack xwmColors
    , inactiveColor       = black xwmColors
    , urgentColor         = red xwmColors
    , activeBorderColor   = bRed xwmColors
    , inactiveBorderColor = black xwmColors
    , urgentBorderColor   = blue xwmColors
    , activeTextColor     = green xwmColors
    , inactiveTextColor   = blue xwmColors
    , urgentTextColor     = yellow xwmColors
    }

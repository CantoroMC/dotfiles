module Config.Theme
    ( Theme (..)
    , xmTheme
    , xmBackGround
    ) where

import XMonad hiding
    ( Color
    , borderWidth
    )

import Config.Color
    ( Colors (..)
    , Color
    , xmColors
    )

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

xmTheme :: Theme
xmTheme = Theme
    { borderWidth         = 3
    , activeColor         = color8 xmColors
    , inactiveColor       = color0 xmColors
    , urgentColor         = color1 xmColors
    , activeBorderColor   = color9 xmColors
    , inactiveBorderColor = color0 xmColors
    , urgentBorderColor   = color4 xmColors
    , activeTextColor     = color2 xmColors
    , inactiveTextColor   = color4 xmColors
    , urgentTextColor     = color3 xmColors
    }

xmBackGround :: String
xmBackGround = "dark"

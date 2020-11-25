module XMonad.Local.Config.Color
    ( Colors (..)
    , Color
    , xmColors
    ) where

type Color = String

data Colors = Colors
    { color0 :: Color
    , color1 :: Color
    , color2 :: Color
    , color3 :: Color
    , color4 :: Color
    , color5 :: Color
    , color6 :: Color
    , color7 :: Color
    , color8 :: Color
    , color9 :: Color
    , colorA :: Color
    , colorB :: Color
    , colorC :: Color
    , colorD :: Color
    , colorE :: Color
    , colorF :: Color
    } deriving (Eq, Read, Show)

xmColors :: Colors
xmColors = Colors
    { -- Black
      color0 = "#151a1e"
    , color8 = "#3f4e5a"
      -- Red
    , color1 = "#ff3333"
    , color9 = "#ff7733"
      -- Green
    , color2 = "#b8cc52"
    , colorA = "#eafe84"
      -- Yellow
    , color3 = "#e7c547"
    , colorB = "#fff779"
      -- Blue
    , color4 = "#36a3d9"
    , colorC = "#68d5ff"
      -- Magenta
    , color5 = "#f07178"
    , colorD = "#ffa3aa"
      -- Cyan
    , color6 = "#95e6cb"
    , colorE = "#c7fffd"
      -- White
    , color7 = "#eaeaea"
    , colorF = "#fafafa"
    }

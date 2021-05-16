module Config.Colors
    ( Colors(..)
    , Color
    , xwmColors
    ) where

type Color = String
data Colors = Colors
    { black    :: Color
    , bBlack   :: Color
    , red      :: Color
    , bRed     :: Color
    , green    :: Color
    , bGreen   :: Color
    , yellow   :: Color
    , bYellow  :: Color
    , blue     :: Color
    , bBlue    :: Color
    , magenta  :: Color
    , bMagenta :: Color
    , cyan     :: Color
    , bCyan    :: Color
    , white    :: Color
    , bWhite   :: Color
    } deriving (Eq, Read, Show)

xwmColors :: Colors
xwmColors = Colors
    { black    = "#151a1e"
    , bBlack   = "#3f4e5a"
    , red      = "#ff3333"
    , bRed     = "#ff7733"
    , green    = "#b8cc52"
    , bGreen   = "#eafe84"
    , yellow   = "#e7c547"
    , bYellow  = "#fff779"
    , blue     = "#36a3d9"
    , bBlue    = "#68d5ff"
    , magenta  = "#f07178"
    , bMagenta = "#ffa3aa"
    , cyan     = "#95e6cb"
    , bCyan    = "#c7fffd"
    , white    = "#eaeaea"
    , bWhite   = "#fafafa"
    }

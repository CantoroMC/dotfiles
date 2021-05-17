module Colors (xBarConfigDir, Palette(..), palette) where


import System.Environment (lookupEnv)



xBarConfigDir :: String
xBarConfigDir = "\"${XMOBAR_CONFIG_DIR}\""

icons :: String -> String
icons bg = xBarConfigDir ++ "/utilities/icons/" ++ bg


data Palette = Palette
    { pFont       :: String
    , pForeground :: String
    , pBackground :: String
    , pAlpha      :: Int
    , pBorder     :: String
    , pIconRoot   :: String
    , pLow        :: String
    , pNormal     :: String
    , pHigh       :: String
    , pIsLight    :: Bool
    }

lightPalette :: Palette
lightPalette = Palette
    { pFont       = "xft:Operator Mono Lig:style=Italic:size=8"
    , pForeground = "#151a1e"
    , pBackground = "#eaeaea"
    , pAlpha      = 255
    , pBorder     = "#36a3d9"
    , pIconRoot   = icons "light"
    , pLow        = "#36a3d9"
    , pNormal     = "#151a1e"
    , pHigh       = "#ff3333"
    , pIsLight    = True
    }

darkPalette :: Palette
darkPalette = Palette
    { pFont       = "xft:Operator Mono Lig:style=Italic:size=8"
    , pForeground = "#eaeaea"
    , pBackground = "#151a1e"
    , pAlpha      = 255
    , pBorder     = "#b8cc52"
    , pIconRoot   = icons "dark"
    , pLow        = "#36a3d9"
    , pNormal     = "#fafafa"
    , pHigh       = "#ff3333"
    , pIsLight    = False
    }


isLight :: IO Bool
isLight = fmap (== Just "light") (lookupEnv "BACKGROUND_COLOR")

palette :: IO Palette
palette = do
    light <- isLight
    if light then return lightPalette else return darkPalette

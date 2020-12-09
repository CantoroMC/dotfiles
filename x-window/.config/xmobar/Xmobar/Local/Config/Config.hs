module Xmobar.Local.Config.Config
    ( Palette (..)
    , palette
    , defaultHeight
    , baseConfig
    , (<~>)
    , (>~<)
    , withPlugArgs
    , fc
    , fn
    , action
    , icon
    , separator
    , xmobarConfigDir
    ) where

------------------------------------------------------------------------------
    -- Imports
import System.Environment (lookupEnv)

import Xmobar

------------------------------------------------------------------------------
    -- Basic Variables

xmobarConfigDir :: String
xmobarConfigDir = "\"${XDG_CONFIG_HOME}\"/xmobar"

icons :: String -> String
icons bg = xmobarConfigDir ++ "/icons/" ++ bg

------------------------------------------------------------------------------
    -- Operators

(<~>) :: Palette -> [String] -> [String]
(<~>) p args =
  args ++ [ "--low", pLow p , "--normal", pNormal p , "--high", pHigh p]

(>~<) :: Palette -> [String] -> [String]
(>~<) p args =
  args ++ [ "--low", pHigh p , "--normal", pNormal p , "--high", pLow p]

withPlugArgs :: Palette -> [String] -> [String] -> [String]
withPlugArgs p args extras = concat [p <~> args, ["--"], extras]

fc :: String -> String -> String
fc color arg = "<fc=" ++ color ++ ">" ++ arg ++ "</fc>"

fn :: Int -> String -> String
fn n arg = "<fn=" ++ show n ++ ">" ++ arg ++ "</fn>"

action :: String -> Int -> String -> String
action com n arg = "<action=`" ++ com ++ "` button=" ++ show n ++ ">" ++ arg ++ "</action>"

icon :: String -> String
icon arg = "<icon=" ++ arg ++ "/>"

separator :: String
separator = icon "separators/separator.xpm"

------------------------------------------------------------------------------
    -- Color Palettes

data Palette = Palette
    { pFont :: String
    , pForeground :: String
    , pBackground :: String
    , pAlpha :: Int
    , pBorder :: String
    , pIconRoot :: String
    , pLow :: String
    , pNormal :: String
    , pHigh :: String
    , pIconColor :: String
    -- , pIsLight :: Bool
    }

lightPalette :: Palette -- TODO
lightPalette = Palette
    { pFont       = "xft:mononoki Nerd Font:style=Italic:pixelsize=9"
    , pForeground = "#151a1e"
    , pBackground = "#eaeaea"
    , pAlpha      = 255
    , pBorder     = "#36a3d9"
    , pIconRoot   = icons "dark"
    , pLow        = "#36a3d9"
    , pNormal     = "#151a1e"
    , pHigh       = "#ff3333"
    , pIconColor  = "#b8cc52"
    }

darkPalette :: Palette
darkPalette = Palette
    { pFont       = "xft:mononoki Nerd Font:style= Italic:pixelsize=9"
    , pForeground = "#eaeaea"
    , pBackground = "#151a1e"
    , pAlpha      = 255
    , pBorder     = "#b8cc52"
    , pIconRoot   = icons "dark"
    , pLow        = "#68d5ff"
    , pNormal     = "#eaeaea"
    , pHigh       = "#ff3333"
    , pIconColor  = "#b8cc52"
    }

isLight :: IO Bool
isLight = fmap (== Just "light") (lookupEnv "BACKGROUND_COLOR")

palette :: IO Palette
palette = do
    light <- isLight
    if light then return lightPalette else return darkPalette

------------------------------------------------------------------------------
    -- Configuration

defaultHeight :: Int
defaultHeight = 22

baseConfig :: Palette -> Config
baseConfig p = defaultConfig
    { font             = pFont p
    , additionalFonts  =
        [ "xft:SauceCodePro Nerd Font:style=Black Italic:size=12:hinting=true"
        , "xft:Ubuntu:weight=bold:pixelsize=8:antialias=true:hinting=true"
        , "xft:FiraCode Nerd Font:style=Regular:pixelsize=10"
        ]
    , bgColor          = pBackground p
    , fgColor          = pForeground p
    , alpha            = pAlpha p
    , borderColor      = pBorder p
    , textOffset       = -1
    , textOffsets      =
        [ -1
        , -1
        , -1
        ]
    , iconOffset       = -1
    , iconRoot         = pIconRoot p
    , lowerOnStart     = True
    , hideOnStart      = False
    , allDesktops      = True
    , overrideRedirect = True
    , pickBroadest     = False
    , persistent       = False
    , sepChar          = "|"
    , alignSep         = "}{"
    }

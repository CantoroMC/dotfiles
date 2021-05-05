module Config
    ( Palette(..)
    , palette
    , baseConfig
    , (<~>)
    , (>~<)
    , withHighArgs
    , withLowArgs
    , fc
    , fn
    , action
    , icon
    , separator
    , xmobarConfigDir
    ) where

import System.Environment (lookupEnv)
import Xmobar


xmobarConfigDir :: String
xmobarConfigDir = "\"${DOTFILES}\"/deploy/third-party/XMonad/xmobarbarian"

icons :: String -> String
icons bg = xmobarConfigDir ++ "/icons/" ++ bg


(<~>) :: Palette -> [String] -> [String]
(<~>) p args =
    args ++ ["--low", pLow p, "--normal", pNormal p, "--high", pHigh p]

(>~<) :: Palette -> [String] -> [String]
(>~<) p args =
    args ++ ["--low", pHigh p, "--normal", pNormal p, "--high", pLow p]

withHighArgs :: Palette -> [String] -> [String] -> [String]
withHighArgs p args extras = concat [p <~> args, ["--"], extras]

withLowArgs :: Palette -> [String] -> [String] -> [String]
withLowArgs p args extras = concat [p >~< args, ["--"], extras]

fc :: String -> String -> String
fc color arg = "<fc=" ++ color ++ ">" ++ arg ++ "</fc>"

fn :: Int -> String -> String
fn n arg = "<fn=" ++ show n ++ ">" ++ arg ++ "</fn>"

action :: String -> Int -> String -> String
action com n arg =
    "<action=`" ++ com ++ "` button=" ++ show n ++ ">" ++ arg ++ "</action>"

icon :: String -> String
icon arg = "<icon=" ++ arg ++ "/>"

separator :: String
separator = icon "separators/separator.xpm"



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
    { pFont       = "xft:Operator Mono Lig:style=Italic:pixelsize=10"
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
    { pFont       = "xft:Operator Mono Lig:style= Italic:pixelsize=10"
    , pForeground = "#fffaf3"
    , pBackground = "#0d0f18"
    , pAlpha      = 255
    , pBorder     = "#8ce00a"
    , pIconRoot   = icons "dark"
    , pLow        = "#00d7eb"
    , pNormal     = "#ffffff"
    , pHigh       = "#ff000f"
    , pIsLight    = False
    }

isLight :: IO Bool
isLight = fmap (== Just "light") (lookupEnv "BACKGROUND_COLOR")

palette :: IO Palette
palette = do
    light <- isLight
    if light then return lightPalette else return darkPalette

baseConfig :: Palette -> Config
baseConfig p = defaultConfig
    { font             = pFont p
    , additionalFonts  =
        [ "xft:SauceCodePro Nerd Font:style=Black Italic:size=10:hinting=true"
        , "xft:Ubuntu:weight=bold:pixelsize=7:antialias=true:hinting=true"
        , "xft:FiraCode Nerd Font:style=Regular:pixelsize=9"
        , "xft:Operator Mono Lig:style=Bold:pixelsize=14"
        ]
    , bgColor          = pBackground p
    , fgColor          = pForeground p
    , alpha            = pAlpha p
    , position         = TopSize C 100 24
    , borderColor      = pBorder p
    , border           = NoBorder
    , textOffset       = -1
    , textOffsets      = [-1, -1, -1]
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

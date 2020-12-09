-- Xmobarcc: http://projects.haskell.org/xmobar

------------------------------------------------------------------------------
    -- Imports

import Xmobar

import Xmobar.Local.Config.Config
    ( Palette (..)
    , palette
    , defaultHeight
    , baseConfig
    , action
    , separator
    )

import Xmobar.Local.Config.Monitors
    ( trayerPad
    , mpdMusic
    , diskIO
    , diskU
    , clock
    , weather
    , memory
    , swap
    , multicpu
    , thermal
    , uptime
    , battery
    , brightness
    , volume
    , keyboard
    , wifi
    , xMenu
    , pacman
    )

------------------------------------------------------------------------------
    -- Configuration

xmobarConfig :: Palette -> Config
xmobarConfig p = (baseConfig p)
    { position = TopSize C 100 defaultHeight
    , border   = BottomB
    , commands =
        [ Run UnsafeStdinReader
        , Run trayerPad
        , Run (mpdMusic p)
        , Run diskU
        , Run diskIO
        , Run weather
        , Run (clock p)
        , Run (memory p)
        , Run swap
        , Run (multicpu p)
        , Run (thermal p)
        , Run uptime
        , Run (battery p)
        , Run (brightness p)
        , Run (volume p)
        , Run keyboard
        , Run (wifi p)
        , Run pacman
        ]
    , template = " "
        ++ xMenu "xmenu-apps" "\58911"
        ++ "|UnsafeStdinReader|" ++ separator
        ++ "|music| " ++ separator
        ++ "|disku| |diskio|"
        ++ "}"
        ++ "|LIML| "
        ++ "|date|"
        ++ action "st sudo pacman -Syu" 3 " |pacman|"
        ++ "{"
        ++ "|memory| |swap| " ++ separator
        ++ "|multicpu| " ++ separator
        ++ "|multicoretemp| " ++ separator
        ++ "|uptime| " ++ separator
        ++ "|battery0| " ++ separator
        ++ "|bright| " ++ separator
        ++ "|default:Master| " ++ separator
        ++ "|kbd| " ++ separator
        ++ "|dynnetwork|" ++ separator
        ++ xMenu "xmenu-shutdown" "\61457"
        ++ "    "
        ++ "|trayerPad|"
    }

------------------------------------------------------------------------------
    -- Main
main :: IO ()
main = palette >>= configFromArgs . xmobarConfig >>= xmobar

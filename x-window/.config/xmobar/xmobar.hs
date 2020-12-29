-- Xmobarcc: http://projects.haskell.org/xmobar

import Xmobar

import Xmobar.Local.Config.Config   ( Palette(..)
                                    , action
                                    , baseConfig
                                    , defaultHeight
                                    , palette
                                    , separator
                                    )

import Xmobar.Local.Config.Monitors ( aur
                                    , battery
                                    , brightness
                                    , clock
                                    , diskIO
                                    , diskU
                                    , keyboard
                                    , memory
                                    , mpdMusic
                                    , multicpu
                                    , pacman
                                    , swap
                                    , thermal
                                    , trayerPad
                                    , uptime
                                    , volume
                                    , weather
                                    , wifi
                                    , xMenu
                                    )

xmobarConfig :: Palette -> Config
xmobarConfig p = (baseConfig p)
  { position = TopSize C 100 defaultHeight
    -- , border   = BottomB
  , border   = NoBorder
  , commands = [ Run UnsafeStdinReader
               , Run trayerPad
               , Run (mpdMusic p)
               , Run diskU
               , Run diskIO
               , Run weather
               , Run (clock p)
               , Run pacman
               , Run aur
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
               ]
  , template = " "
               ++ xMenu "xmenu-apps" "\58911"
               ++ "|UnsafeStdinReader| "
               ++ separator
               ++ "|music| "
               ++ separator
               ++ "|disku| |diskio|"
               ++ "}"
               ++ "|LIML| "
               ++ "|date|"
               ++ "{"
               ++ action "st yay -Syu" 3 " |pacman| |aur|"
               ++ separator
               ++ "|memory| |swap| "
               ++ separator
               ++ "|multicpu| "
               ++ separator
               ++ "|multicoretemp| "
               ++ separator
               ++ "|uptime| "
               ++ separator
               ++ "|battery0| "
               ++ separator
               ++ "|bright| "
               ++ separator
               ++ "|default:Master| "
               ++ separator
               ++ "|kbd| "
               ++ separator
               ++ "|dynnetwork|"
               ++ separator
               ++ xMenu "xmenu-shutdown" "\61457"
               ++ "    "
               ++ "|trayerPad|"
  }

main :: IO ()
main = palette >>= configFromArgs . xmobarConfig >>= xmobar

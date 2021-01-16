-- Xmobarcc: http://projects.haskell.org/xmobar

import Xmobar

import Xmobar.Local.Config.Config   ( Palette(..)
                                    , action
                                    , baseConfig
                                    , defaultHeight
                                    , palette
                                    )

import Xmobar.Local.Config.Monitors ( aur
                                    , battery
                                    , brightness
                                    , clock
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
               ++ " "
               ++ "|music| "
               ++ "}"
               ++ "|LIML| "
               ++ "|date|"
               ++ "{"
               ++ action "st yay -Syu" 3 " |pacman| |aur|"
               ++ " "
               ++ "|memory| |swap|"
               ++ " "
               ++ "|multicpu|"
               ++ " "
               ++ "|multicoretemp|"
               ++ " "
               ++ "|uptime| "
               ++ " "
               ++ "|battery0|"
               ++ " "
               ++ "|bright|"
               ++ " "
               ++ "|default:Master|"
               ++ " "
               ++ "|kbd|"
               ++ " "
               ++ "|dynnetwork|"
               ++ " "
               ++ xMenu "xmenu-shutdown" "\61457"
               ++ "      "
               ++ "|trayerPad|"
  }

main :: IO ()
main = palette >>= configFromArgs . xmobarConfig >>= xmobar

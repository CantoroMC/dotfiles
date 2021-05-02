import Xmobar
    ( configFromArgs,
      xmobar,
      Config(commands, template),
      StdinReader(UnsafeStdinReader),
      Runnable(Run) )

import Config (Palette(..), action, baseConfig, palette)
import Monitors
    ( battery
    , brightness
    , clock
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
    , xmSound
    )

xmobarConfig :: Palette -> Config
xmobarConfig p = (baseConfig p)
    { commands =
        [ Run UnsafeStdinReader
        , Run trayerPad
        , Run (mpdMusic p)
        , Run weather
        , Run (clock p)
        , Run pacman
        , Run xmSound
        , Run (memory p)
        , Run swap
        , Run (multicpu p)
        , Run (thermal p)
        , Run uptime
        , Run (battery p "BAT0" "battery0")
        , Run (brightness p)
        , Run (volume p)
        , Run (wifi p)
        ]
    , template =
        " "
        ++ xMenu "xmenu-apps" "\58911" "#8ce00a"
        ++ "|UnsafeStdinReader| "
        ++ " "
        ++ "|music| "
        ++ "}"
        ++ "|LIML| "
        ++ "|date|"
        ++ "{"
        ++ action "st sudo pacman -Syu" 3 " |pacman|"
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
        ++ action "pactl set-sink-volume @DEFAULT_SINK@ -5%" 5
               (action "pactl set-sink-volume @DEFAULT_SINK@ +5%" 4
                   (action "pactl set-sink-mute @DEFAULT_SINK@ toggle" 2
                       (action "st -n volume -t volume pulsemixer" 1
                           (action "pavucontrol" 3 "|xmVolume|")
                       )
                   )
               )
        ++ " "
        ++ "|dynnetwork|"
        ++ " "
        ++ xMenu "xmenu-shutdown" "\61457" "#008df8"
        ++ " "
        ++ "   |trayerPad|"
    }

main :: IO ()
main = palette >>= configFromArgs . xmobarConfig >>= xmobar

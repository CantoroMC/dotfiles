import Xmobar

import Colors (Palette(..), palette)
import Config (baseConfig)
import Monitors
    ( trayerPad
    , pacman
    , sound
    , battery
    , brightness
    , dynNet
    , memory
    , mpdMusic
    , multicpu
    , swap
    , thermal
    , uptime
    , weather
    , clock
    , keyboard
    )
import Util (action)


xBarConfig :: Palette -> Config
xBarConfig p = (baseConfig p)
    { commands =
        [ Run UnsafeXMonadLog
        , Run trayerPad
        , Run pacman
        , Run sound
        , Run (battery p "BAT0" "battery0")
        , Run (brightness p)
        , Run (dynNet p)
        , Run (memory p)
        , Run (mpdMusic p)
        , Run (multicpu p)
        , Run swap
        , Run (thermal p)
        , Run uptime
        , Run (weather p)
        , Run clock
        , Run keyboard
        ]
    , template =
        "|UnsafeXMonadLog|"
        ++ " |music|"
        ++ action "st sudo pacman -Syu" 3 " |pacman|"
        ++ "}"
        ++ "|date|"
        ++ " |LIML|"
        ++ "{"
        ++ "|multicpu|"
        ++ " |multicoretemp|"
        ++ " |memory|(|swap|)"
        ++ "   "
        ++ "|dynnetwork|"
        ++ "   "
        ++ "|uptime|"
        ++ " |kbd|"
        ++ action "pactl set-sink-volume @DEFAULT_SINK@ -5%" 5
               (action "pactl set-sink-volume @DEFAULT_SINK@ +5%" 4
                   (action "pactl set-sink-mute @DEFAULT_SINK@ toggle" 2
                       (action "st -n volume -t volume pulsemixer" 1
                           (action "pavucontrol" 3 " |sound|")
                       )
                   )
               )
        ++ " |bright|"
        ++ " |battery0|"
        ++ "   "
        ++ "|trayerPad|"
    }

main :: IO ()
main = palette >>= configFromArgs . xBarConfig >>= xmobar

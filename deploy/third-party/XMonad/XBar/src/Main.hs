import Xmobar

import Colors (Palette(..), palette)
import Config (baseConfig)
import Monitors
    ( trayerPad
    , pacman
    , sound
    , battery
    , weather
    , dynNet
    , wireless
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
        , Run (weather p)
        , Run (dynNet p)
        , Run wireless
        , Run (clock p)
        , Run keyboard
        , Run $ Cpu
            [ "-L", "3"
            , "-H", "50"
            , "--normal", "green"
            , "--high", "red"
            ] 10
        , Run $ Memory ["-t","Mem: <usedratio>%"] 10
        , Run $ Swap [] 10
        ]
    , template =
        " "
        ++ "|UnsafeXMonadLog| "
        ++ "}"
        ++ "|LIML|"
        ++ " |date|"
        ++ "{"
        ++ action "st sudo pacman -Syu" 3 " |pacman|"
        ++ " |cpu| - |memory| * |swap| - |wlan0wi|"
        ++ action "pactl set-sink-volume @DEFAULT_SINK@ -5%" 5
               (action "pactl set-sink-volume @DEFAULT_SINK@ +5%" 4
                   (action "pactl set-sink-mute @DEFAULT_SINK@ toggle" 2
                       (action "st -n volume -t volume pulsemixer" 1
                           (action "pavucontrol" 3 " |sound|")
                       )
                   )
               )
        ++ " |battery0|"
        ++ " |kbd|"
        ++ " |dynnetwork|"
        ++ "  "
        ++ " |trayerPad|"
    }

main :: IO ()
main = palette >>= configFromArgs . xBarConfig >>= xmobar

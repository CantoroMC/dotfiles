module Monitors
    (   -- * Commands
        -- $commands
      trayerPad
    , pacman
    , sound
        -- * Monitors
        -- $monitors
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
        -- * Others
        -- $others
    , clock
    , keyboard
    ) where



import Xmobar

import Colors (xBarConfigDir, Palette(..))
import Util
    ( withHighArgs
    , withLowArgs
    , action
    , fc
    , fn
    , icon
    )



-------------------------------------------------------------------------------
    -- Commands
trayerPad :: Command
trayerPad = Com "/bin/sh" ["-c", xBarConfigDir ++ "/utilities/scripts/padding-icon"] "trayerPad" 60

pacman :: Command
pacman = Com "/bin/sh" ["-c", xBarConfigDir ++ "/utilities/scripts/xmPacman"] "pacman" 600

sound :: Command
sound = Com "/bin/sh" ["-c", xBarConfigDir ++ "/utilities/scripts/xmVolume"] "sound" 10

-------------------------------------------------------------------------------
    -- Monitors

battery :: Palette -> String -> Alias -> Monitors
battery p b = BatteryN [b]
    ( withLowArgs p
        [ "--template", "<leftipat>" ++ fn 2 " <acstatus>"
        , "--suffix", "True"
        ]
        [ "--on-icon-pattern",   icon "battery/on/battery_on_%%.xpm"
        , "--off-icon-pattern",  icon "battery/off/battery_off_%%.xpm"
        , "--idle-icon-pattern", icon "battery/idle/battery_idle_%%.xpm"
        , "-o" , fn 2 "<left> (<timeleft>)"
        , "-O" , fn 2 "<left> (<timeleft>)"
        , "-i" , fn 2 "IDLE <left>"
        , "-a" , "notify-send -u critical 'Battery Low'"
        , "-A" , "7"
        ]
    ) 400

brightness :: Palette -> Monitors
brightness p = Brightness
    (withLowArgs p
        [ "--template", action "xbacklight -inc 5" 4
            (action "xbacklight -dec 5" 5
            (action "xbacklight -set 100" 3
            (action "xbacklight -set 0" 2
            ("<ipat>" ++ fn 2 " <percent>"))))
        , "--suffix", "True"
        ]
        [ "-D", "intel_backlight"
        , "--brightness-icon-pattern", icon "brightness/brightness_%%.xpm"
        ]
    ) 10

dynNet :: Palette -> Monitors
dynNet p = DynNetwork
    ( withHighArgs p
        [ "--template", fn 2 "<dev> <txipat><tx> <rxipat><rx>"
        , "--suffix", "True"
        , "--High", "2000"
        , "--Low",  "20"
        , "--ddigits", "1"
        ]
        [ "--rx-icon-pattern", icon "network/rx/network_rx_%%.xpm"
        , "--tx-icon-pattern", icon "network/tx/network_tx_%%.xpm"
        ]
    ) 10

memory :: Palette -> Monitors
memory p = Memory
    ( withHighArgs p
        [ "--template", action "kitty -e htop" 3 ("<usedipat>" ++ fn 2 " <usedratio>")
        , "--ppad", "2"
        , "--suffix", "True"
        ]
        ["--used-icon-pattern", "<icon=ram/ram_%%.xpm/>"]
    ) 10

mpdMusic :: Palette -> Monitors
mpdMusic p = MPDX
    (withHighArgs p
        [ "--template", "<statei> "
        , "--maxwidth", "12"
        ]
        [ "-P", action "mpc prev"  1 (icon "music/music_prev.xpm")
            ++ action  "mpc pause" 1 (action "mpc stop" 3 (icon "music/music_paused.xpm"))
            ++ action  "mpc next"  1 (icon "music/music_next.xpm")
            ++ action "wmctrl -xR ncmpcpp" 3 (fn 2" <artist>-<title>")
            ++ action "mpc seek +1%"   4
                (action "mpc seek -1%" 5 (fn 2 " [<lapsed>/<length>]"))
            ++ action "mpc volume +3"      4
                (action "mpc volume -3" 5 (fn 3 " 墳" ++ fn 2 " <volume>%"))
        , "-Z", action "mpc play" 1 (action "mpc stop" 3 (icon "music/music_playing.xpm"))
            ++ action "wmctrl -xR ncmpcpp" 3 (fn 2" <artist>-<title>")
            ++ action "mpc seek +1%" 4
                (action "mpc seek -1%" 5 (fn 2 " [<lapsed>/<length>]"))
            ++ action "mpc volume +3" 4
                (action "mpc volume -3" 5 (fn 3 " ﱛ" ++ fn 2 " <volume>%"))
        , "-S" , action "mpc play" 1 (fn 3 "\63622 " ++ icon "music/music_stopped.xpm")
        ]
    ) 10 "music"

multicpu :: Palette -> Monitors
multicpu p = MultiCpu
    ( withHighArgs p
        [ "--template", "<ipat>" ++
            fn 2 "<total0><total1><total2><total3><total4><total5><total6><total7>"
        , "--suffix",   "True"
        , "--ppad",     "3"
        , "--ddigits",  "0"
        , "--minwidth", "4"
        , "--align" ,   "l"
        ]
        [ "--load-icon-pattern", icon "cpu/cpu_%%.xpm"
        ]
    ) 10

swap :: Monitors
swap = Swap
    [ "--template", fn 2 "<usedratio>"
    , "--ppad", "2"
    , "--suffix", "True"
    ] 10

thermal :: Palette -> Monitors
thermal p = MultiCoreTemp
    ( withHighArgs p
        [ "--template", "<maxipat>" ++ fn 2 " <max>°C"
        ]
        [ "--max-icon-pattern", icon "temperature/temperature_%%.xpm"
        , "--mintemp", "55"
        , "--maxtemp", "100"
        ]
    ) 50

uptime :: Monitors
uptime = Uptime
    [ "--template", fn 3 "羽" ++ fn 1 "<hours>:<minutes>"
    , "--width", "3"
    , "--suffix", "True"
    ] 60

weather :: Palette -> Monitors
weather p = WeatherX
    "LIML"
    [ ("clear"                  , icon "weather/weather_sunny.xpm")
    , ("mostly clear"           , icon "weather/weather_mostly_sunny.xpm")
    , ("sunny"                  , icon "weather/weather_sunny.xpm")
    , ("mostly sunny"           , icon "weather/weather_mostly_sunny.xpm")
    , ("partly sunny"           , icon "weather/weather_mostly_cloudy.xpm")
    , ("cloudy"                 , icon "weather/weather_cloudy.xpm")
    , ("mostly cloudy"          , icon "weather/weather_mostly_cloudy.xpm")
    , ("partly cloudy"          , icon "weather/weather_mostly_sunny.xpm")
    , ("fair"                   , icon "weather/weather_sunny.xpm")
    , ("overcast"               , icon "weather/weather_cloudy.xpm")
    , ("considerable cloudiness", icon "weather/weather_cloudy.xpm")
    , ("obscured"               , icon "weather/weather_obscured.xpm")
    ]
    [ "--template", action "weather" 3
        ( "<skyConditionS><weather> <tempC>°C "
        ++ fc (pLow p) "<rh>% "
        ++ fn 3 "\57982 "
        ++ "<windKmh>km/h"
        )
    ] 100

-------------------------------------------------------------------------------
    -- Others
clock :: Date
clock = Date
    ( action "emacs -e calendar --name 'Orgenda'" 3 ("%T" ++ " - " ++ "%a %e %b")
    ) "date" 10

keyboard :: Kbd
keyboard = Kbd
    [ ("us",          fn 4 "\63506" ++ fn 1 " US")
    , ("it(winkeys)", fn 4 "\63506" ++ fn 1" IT")
    , ("de(qwerty)",  fn 4 "\63506" ++ fn 1" DE")
    ]

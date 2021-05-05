module Monitors
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
    , xmSound
    ) where

import Xmobar

import Config
    ( Palette(..)
    -- , (<~>)
    -- , (>~<)
    , action
    , fc
    , fn
    , icon
    , withHighArgs
    , withLowArgs
    , xmobarConfigDir
    )


-------------------------------------------------------------------------------
    -- Commands
trayerPad :: Command
trayerPad = Com (xmobarConfigDir ++ "/scripts/padding-icon") [] "trayerpad" 3600

pacman :: Command
pacman = Com "/bin/sh" ["-c", xmobarConfigDir ++ "/scripts/xmPacman"] "pacman" 600

xmSound :: Command
xmSound = Com "/bin/sh" ["-c", xmobarConfigDir ++ "/scripts/xmVolume"] "xmVolume" 10

-------------------------------------------------------------------------------
    -- Monitors
mpdMusic :: Palette -> Monitors
mpdMusic p = MPDX
    (withHighArgs p
        [ "--template", "<statei> "
            ++ action "wmctrl -xR ncmpcpp" 3 "<artist>-<title>"
            ++ action "mpc random"         1 (fn 3 " \61556 ")
            ++ action "mpc seek +1%"       4
                (action "mpc seek -1%"  5 (fc "#008df8" "[<lapsed>/<length>]"))
            ++ action "mpc volume +3"      4
                (action "mpc volume -3" 5 (fn 3 " 墳 " ++ "<volume>%"))
        , "--maxwidth", "12"
        ]
        [ "-P"
        , action  "mpc prev"   1 (fn 3 " 玲")
        ++ action "mpc pause"  1 (action "mpc stop" 3 (fn 3 " \61516 "))
        ++ action "mpc next"   1 (fn 3 "怜")
        , "-Z" , action "mpc play"    1 (action "mpc stop" 3 (icon "music/music_playing.xpm"))
        , "-S" , action "mpc play"    1 (icon "music/music_stopped.xpm")
        ]
    ) 10 "music"

diskIO :: Monitors
diskIO = DiskIO [("/", action "nautilus" 3 "(R:<read> W:<write>)")] [] 10

diskU :: Monitors
diskU = DiskU
    [("/", action "gnome-disks" 3 (fc "#b8cc52" "Disk: " ++ "<free>/<size>"))]
    ["--suffix", "True", "-a", "l"]
    10

weather :: Monitors
weather = WeatherX
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
    [ "--template"
    , action
        "weather"
        3
        (  " <skyConditionS> <tempC>°C "
        ++ fc "#b8cc52" "<rh>% "
        ++ fn 1 "\57982 "
        ++ "<windKmh> km/h <weather>"
        )
    ]
    100

memory :: Palette -> Monitors
memory p = Memory
    (withHighArgs
        p
        [ "--template"
        , action "st htop" 3 (fn 2 "<usedipat> <usedratio>")
        , "-p"
        , "2"
        , "--suffix"
        , "True"
        ]
        ["--used-icon-pattern", "<icon=ram/ram_%%.xpm/>"]
    )
    10

swap :: Monitors
swap = Swap ["--template", fn 2 "(<usedratio>)", "-p", "2", "--suffix", "True"] 10

multicpu :: Palette -> Monitors
multicpu p = MultiCpu
    (withHighArgs
        p
        [ "--template", "<ipat>" ++ fn 2 "<total0><total1><total2><total3><total4><total5><total6><total7>"
        , "--suffix" , "True"
        , "-p" , "3"
        , "-d" , "0"
        , "-m" , "4"
        , "--width" , "6"
        , "-a" , "l"
        ]
        ["--load-icon-pattern", icon "cpu/cpu_%%.xpm"]
    )
    10

thermal :: Palette -> Monitors
thermal p = MultiCoreTemp
    (withHighArgs
        p
        ["--template", fn 2 "<maxipat> <max>°C"]
        [ "--max-icon-pattern"
        , icon "temperature/temperature_%%.xpm"
        , "--mintemp"
        , "55"
        , "--maxtemp"
        , "100"
        ]
    )
    50

uptime :: Monitors
uptime = Uptime
    [ "--template", fc "#8ce00a" "Up:" ++ "<hours><minutes>"
    , "--width", "3"
    , "--suffix", "True"
    ] 60

battery :: Palette -> String -> Alias -> Monitors
battery p a = BatteryN
    [a]
    (withLowArgs p
        [ "--template", "<fn=2><leftipat> <acstatus></fn>"
        , "--suffix", "True"
        ]
        [ "--on-icon-pattern",   icon "battery/on/battery_on_%%.xpm"
        , "--off-icon-pattern",  icon "battery/off/battery_off_%%.xpm"
        , "--idle-icon-pattern", icon "battery/idle/battery_idle_%%.xpm"
        , "-o" , "<left> (<timeleft>)"
        , "-O" , "<left> (<timeleft>)"
        , "-i" , "IDLE <left>"
        , "-a" , "notify-send -u critical 'Battery low'"
        , "-A" , "3"
        ]
    )
    400

brightness :: Palette -> Monitors
brightness p = Brightness
    (withLowArgs
        p
        [ "--template"
        , action
            "xbacklight -inc 5"
            4
            (action
                "xbacklight -dec 5"
                5
                (action
                    "xbacklight -set 100"
                    3
                    (action "xbacklight -set 0" 2 ("<ipat>" ++ fn 2 "<percent>")
                    )
                )
            )
        , "--suffix"
        , "True"
        ]
        [ "-D"
        , "intel_backlight"
        , "--brightness-icon-pattern"
        , icon "brightness/brightness_%%.xpm"
        ]
    )
    10

volume :: Palette -> Monitors
volume p = Volume
    "default"
    "Master"
    (withHighArgs
        p
        [ "--template"
        , action
            "pactl set-sink-volume @DEFAULT_SINK@ -5%"
            5
            (action
                "pactl set-sink-volume @DEFAULT_SINK@ +5%"
                4
                (action
                    "pactl set-sink-mute @DEFAULT_SINK@ toggle"
                    2
                    (action
                        "st -n volume -t volume pulsemixer"
                        1
                        (action "pavucontrol" 3 (fn 2 "<status><volume>"))
                    )
                )
            )
        , "--suffix"
        , "True"
        ]
        [ "-O"
        , ""
        , "-o"
        , icon "volume/mute.xpm"
        , "-h"
        , icon "volume/high.xpm"
        , "-m"
        , icon "volume/medium.xpm"
        , "-l"
        , icon "volume/low.xpm"
        ]
    )
    10

wifi :: Palette -> Monitors
wifi p = DynNetwork
    (withHighArgs
        p
        ["--template", fn 2 "<txipat><rxipat>", "--suffix", "True", "-d", "1"]
        [ "--rx-icon-pattern"
        , icon "network/rx/network_rx_%%.xpm"
        , "--tx-icon-pattern"
        , icon "network/tx/network_tx_%%.xpm"
        ]
    )
    10


-------------------------------------------------------------------------------
    -- Others
clock :: Palette -> Date
clock p = Date
    (action
        "emacs -e calendar --name 'orgenda'"
        3
        (fc (pLow p) "%T" ++ " - " ++ fc (pBorder p) "%a %e %b %Y")
    )
    "date"
    10

keyboard :: Kbd
keyboard = Kbd
    [ ( "us"
      , action
          "setxkbmap it; xmodmap ~/.config/X11/xinit/.XmodmapIT"
          3
          (fn 2 "US")
      )
    , ( "it"
      , action
          "setxkbmap us; xmodmap ~/.config/X11/xinit/.Xmodmap"
          3
          (fn 2 "IT")
      )
    ]

xMenu :: String -> String -> String -> String
xMenu com ic color = action com 1 (fn 1 (fc color (ic ++ " ")))

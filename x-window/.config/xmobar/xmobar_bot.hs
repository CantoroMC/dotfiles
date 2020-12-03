Config
    { font = "xft:Ubuntu:weight=bold:pixelsize=11:antialias=true:hinting=true"
    , additionalFonts =
        [ "xft:SauceCodePro Nerd Font:style=Black Italic :size=9:hinting=true"
        , "xft:mononoki Bold Nerd Font:pixelsize=10:antialias=true:hinting=true"
        ]
    , bgColor         = "#151a1e"
    , fgColor         = "#eaeaea"
    , alpha           = 180
    , position        = Bottom
    , border          = NoBorder
    , borderColor     = "#F07178"
    , textOffset      = -1
    , iconOffset      = -1
    , iconRoot        = "/home/cantoro/.config/xmobar/icons"

    -- general behavior
    , lowerOnStart     = True   -- send to bottom of window stack on start
    , hideOnStart      = False  -- start with window unmapped (hidden)
    , allDesktops      = True   -- show on all desktops
    , overrideRedirect = True   -- set the Override Redirect flag (Xlib)
    , pickBroadest     = False  -- choose widest display (multi-monitor)
    , persistent       = False  -- enable/disable hiding (True = disabled)

    -- plugins
    , commands =
        [ Run Swap
            [ "--t", "Swap <usedratio>"
            , "-p", "2"
            , "-S", "True"
            ] 10
        , Run Uptime
            [ "-t", "Up: <hours> <minutes>"
            , "-w", "3"
            , "-S", "True"
            ] 60
        , Run DiskIO [("/", "<action=`thunar` button=3><fn=1>\61888 </fn> R:<read> W:<write></action>")] [] 10
        , Run DiskU  [("/", "<action=`gnome-disks` button=3><fn=1>\63433 </fn><free>/<size> (<usedp>)</action>")]
            [ "-S", "True"
            , "-a", "l"
            ] 20
        , Run WeatherX "LIML"
            [ ("clear"                   , "<icon=weather/weather_sunny.xpm/>")
            , ("mostly clear"            , "<icon=weather/weather_mostly_sunny.xpm/>")
            , ("sunny"                   , "<icon=weather/weather_sunny.xpm/>")
            , ("mostly sunny"            , "<icon=weather/weather_mostly_sunny.xpm/>")
            , ("partly sunny"            , "<icon=weather/weather_mostly_cloudy.xpm/>")
            , ("cloudy"                  , "<icon=weather/weather_cloudy.xpm/>")
            , ("mostly cloudy"           , "<icon=weather/weather_mostly_cloudy.xpm/>")
            , ("partly cloudy"           , "<icon=weather/weather_mostly_sunny.xpm/>")
            , ("fair"                    , "<icon=weather/weather_sunny.xpm/>")
            , ("overcast"                , "<icon=weather/weather_cloudy.xpm/>")
            , ("considerable cloudiness" , "<icon=weather/weather_cloudy.xpm/>")
            , ("obscured"                , "<icon=weather/weather_obscured.xpm/>")
            ]
            [ "--t" , "<action=`weather` button=3> <skyConditionS> <tempC>°C <fc=#b8cc52><rh>%</fc> <fn=1>\57982 </fn><windKmh> km/h <weather></action>"
            ] 100
        , Run MPD
            [ "-t", "<statei> <fc=#b8cc52><action=`wmctrl -xR ncmpcpp` button=3><artist>-<title></action></fc> <action=`mpc random` button=1><fn=1>\61556 </fn></action><fc=#ff3333> <action=`mpc seek +1%` button=4><action=`mpc seek -1%` button=5>[<lapsed>/<length>]</action></action> <action=`mpc volume +3` button=4><action=`mpc volume -3` button=5>(<fn=1>墳 </fn><volume>%)</action></action></fc>"
            , "-M", "15"
            , "--"
                , "-P", "<fc=#b8cc52><action=`mpc prev` button=1><fn=1>玲</fn></action> <action=`mpc pause` button=1><action=`mpc stop` button=3> <fn=1>\61516 </fn></action></action><action=`mpc next` button=1><fn=1>怜</fn> </action></fc>"
                , "-Z", "<action=`mpc play` button=1><action=`mpc stop` button=3><icon=music/music_playing.xpm/></action></action>"
                , "-S", "<action=`mpc play` button=1><icon=music/music_stopped.xpm/></action>"
            ] 10
        , Run Kbd
            [ ("us", "<action=`setxkbmap it; xmodmap ~/.config/X11/xinit/.XmodmapIT` button=3>US</action>")
            , ("it", "<action=`setxkbmap us; xmodmap ~/.config/X11/xinit/.Xmodmap` button=3>IT</action>")
            ]
        ]

    -- layout
    , sepChar =  "*"
    , alignSep = "}{"
    , template =
        "*swap*  \
        \ *uptime*  \
        \ *diskio*  \
        \ *disku*}\
        \ *LIML*\
        \{*mpd* <icon=separators/separator.xpm/> \
        \ *kbd* "
}

-- vim:nospell

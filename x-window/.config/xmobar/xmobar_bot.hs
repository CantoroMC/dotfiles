Config
    { font = "xft:Ubuntu:weight=bold:pixelsize=11:antialias=true:hinting=true"
    , additionalFonts =
        [ "xft:SauceCodePro Nerd Font:style=Black Italic :size=9:hinting=true"
        , "xft:mononoki Bold Nerd Font:pixelsize=10:antialias=true:hinting=true"
        ]
    , bgColor         = "#151a1e"
    , fgColor         = "#B8CC52"
    , alpha           = 180
    , position        = Bottom
    , border          = NoBorder
    , borderColor     = "#F07178"
    , textOffset      = -1
    , iconOffset      = -1
    , iconRoot        = "/home/cantoro/.config/xmobar/icons"

    -- general behavior
    , lowerOnStart = True     -- send to bottom of window stack on start
    , hideOnStart = False     -- start with window unmapped (hidden)
    , allDesktops = True      -- show on all desktops
    , overrideRedirect = True -- set the Override Redirect flag (Xlib)
    , pickBroadest = False    -- choose widest display (multi-monitor)
    , persistent = False      -- enable/disable hiding (True = disabled)

    -- plugins
    , commands =
        [ Run Swap
            [ "--t", "<fc=#EAEAEA>Swap</fc><usedratio>"
            , "-L", "5"
            , "-H", "80"
            , "-l", "#F07178"
            , "-n", "#FF7733"
            , "-h", "#FF3333"
            , "-p", "2"
            , "-S", "True"
            ] 10
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
            [ "--t" , "<action=`weather` button=3> <skyConditionS> <tempC>°C <fc=#36A3D9><rh>%</fc> <fn=1>\57982 </fn><fc=#36A3D9><windKmh> km/h</fc> <weather></action>"
            ] 100
        , Run MPD
            [ "-t", "<statei> <fc=#36A3D9><action=`wmctrl -xR ncmpcpp` button=3><artist>-<title></action></fc> <action=`mpc random` button=1><fn=1>\61556 </fn></action><fc=#B8CC52> <action=`mpc seek +1%` button=4><action=`mpc seek -1%` button=5>[<lapsed>/<length>]</action></action> <action=`mpc volume +3` button=4><action=`mpc volume -3` button=5>(<fn=1>墳 </fn><volume>%)</action></action></fc>"
            , "-M", "15"
            , "--"
                , "-P", "<fc=#36A3D9><action=`mpc prev` button=1><fn=1>玲</fn></action> <action=`mpc pause` button=1><action=`mpc stop` button=3> <fn=1>\61516 </fn></action></action><action=`mpc next` button=1><fn=1>怜</fn> </action></fc>"
                , "-Z", "<action=`mpc play` button=1><action=`mpc stop` button=3><icon=music/music_playing.xpm/></action></action>"
                , "-S", "<action=`mpc play` button=1><icon=music/music_stopped.xpm/></action>"
            ] 10
        , Run Kbd
            [ ("us", "<action=`setxkbmap it; xmodmap ~/.config/X11/xinit/.XmodmapIT` button=3><fc=#36A3D9>US</fc></action>")
            , ("it", "<action=`setxkbmap us; xmodmap ~/.config/X11/xinit/.Xmodmap` button=3><fc=#36A3D9>IT</fc></action>")
            ]
        , Run Uptime
            [ "-t", "<fc=#EAEAEA>Up:</fc><hours> <minutes>"
            , "-w", "3"
            , "-S", "True"
            ] 60
        , Run DiskIO [("/", "<action=`thunar` button=3><fn=1>\61888 </fn> R:<read> W:<write></action>")] [] 10
        , Run DiskU  [("/", "<action=`gnome-disks` button=3><fn=1>\63433 </fn><free>/<size> (<usedp>)</action>")]
            [ "-L", "10"
            , "-H", "90"
            , "-l", "#FF3333"
            , "-n", "#95E6CB"
            , "-h", "#B8CC52"
            , "-S", "True"
            , "-a", "l"
            ] 20
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

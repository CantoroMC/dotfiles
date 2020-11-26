Config {
    -- appearance
    font = "xft:SauceCodePro Nerd Font:style=Black Italic :size=8:hinting=true,Font Awesome 5 Free Solid:style=Solid:size=15"
    , additionalFonts = []
    , bgColor = "#151a1e"
    , fgColor = "#B8CC52"
    , alpha = 180
    , position = Bottom
    , border = NoBorder
    , borderColor = "#F07178"
    , textOffset = 14
    , textOffsets = []
    , iconOffset = -1
    , iconRoot = "."

    -- general behavior
    , lowerOnStart = True     -- send to bottom of window stack on start
    , hideOnStart = False     -- start with window unmapped (hidden)
    , allDesktops = True      -- show on all desktops
    , overrideRedirect = True -- set the Override Redirect flag (Xlib)
    , pickBroadest = False    -- choose widest display (multi-monitor)
    , persistent = False      -- enable/disable hiding (True = disabled)

    -- plugins
    , commands =
        [ Run Memory
            [ "--t", "<action=`st htop` button=3><fc=#EAEAEA>Mem</fc> <usedratio></action>"
            , "-L", "40"
            , "-H", "90"
            , "-l", "#F07178"
            , "-n", "#B8CC52"
            , "-h", "#FF3333"
            , "-p", "2"
            , "-S", "True"
            ] 10
        , Run Swap
            [ "--t", "<fc=#EAEAEA>Swap</fc><usedratio>"
            , "-L", "5"
            , "-H", "80"
            , "-l", "#F07178"
            , "-n", "#FF7733"
            , "-h", "#FF3333"
            , "-p", "2"
            , "-S", "True"
            ] 10
        , Run Uptime
            [ "-t", "<fc=#EAEAEA>Up:</fc><hours> <minutes>"
            , "-w", "3"
            , "-S", "True"
            ] 60
        , Run DiskIO [("/", "<action=`thunar` button=3>\61888  R:<read> W:<write></action>")] [] 10
        , Run DiskU  [("/", "<action=`gnome-disks` button=3>\63433 <free>/<size> (<usedp>)</action>")]
            [ "-L", "10"
            , "-H", "90"
            , "-l", "#FF3333"
            , "-n", "#95E6CB"
            , "-h", "#B8CC52"
            , "-S", "True"
            , "-a", "l"
            ] 20
        , Run DynNetwork
            [ "-t", "<fc=#EAEAEA>\61931  <dev></fc> \61538 <tx> \61539 <rx>"
            , "-L", "0"
            , "-H", "5000"
            , "-l", "#FF3333"
            , "-n", "#95E6CB"
            , "-h", "#B8CC52"
            , "-m", "8"
            , "-S", "True"
            , "-d", "1"
            ] 10
        ]

    -- layout
    , sepChar =  "*"
    , alignSep = "}{"
    , template = "*memory*   *swap*   *uptime*   *diskio* }\
        \{ *disku*   *dynnetwork*"
}

-- vim:nospell

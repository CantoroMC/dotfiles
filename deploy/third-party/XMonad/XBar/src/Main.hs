import Xmobar

config :: Config
config = defaultConfig
    { font = "xft:Operator Mono Lig:style=Italic:pixelsize=10"
    , additionalFonts = []
    , borderColor = "black"
    , border = TopB
    , bgColor = "black"
    , fgColor = "grey"
    , alpha = 255
    , position         = TopSize C 100 24
    , textOffset = -1
    , iconOffset = -1
    , lowerOnStart = True
    , pickBroadest = False
    , persistent = False
    , hideOnStart = False
    , iconRoot = "."
    , allDesktops = True
    , overrideRedirect = True
    , commands =
        [ Run XMonadLog
        , Run $ Weather "EGPH"
            [ "-t","<station>: <tempC>C"
            , "-L","18"
            , "-H","25"
            , "--normal", "green"
            , "--high", "red"
            , "--low", "lightblue"
            ] 36000
        , Run $ Network "eth0"
            [ "-L", "0"
            , "-H", "32"
            , "--normal", "green"
            , "--high", "red"
            ] 10
        , Run $ Network "eth1"
            [ "-L", "0"
            , "-H", "32"
            , "--normal", "green"
            , "--high", "red"
            ] 10
        , Run $ Cpu
            [ "-L", "3"
            , "-H", "50"
            , "--normal", "green"
            , "--high", "red"
            ] 10
        , Run $ Memory ["-t","Mem: <usedratio>%"] 10
        , Run $ Swap [] 10
        , Run $ Com "uname" ["-s","-r"] "" 36000
        , Run $ Date "%a %b %_d %Y %H:%M:%S" "date" 10
        ]
    , sepChar = "%"
    , alignSep = "}{"
    , template =
        "%XMonadLog% } " ++
        " <fc=#ee9a00>%date%</fc>| %EGPH% | %uname%" ++
        "{ %cpu% | %memory% * %swap% | %eth0% - %eth1%"
}

main :: IO ()
main = xmobar config

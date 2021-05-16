module Monitors
    (   -- * Commands
        -- $commands
      trayerPad
    , pacman
    , sound
        -- * Monitors
        -- $monitors
    , weather
    , dynNet
    , wireless
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
trayerPad = Com (xBarConfigDir ++ "/utilities/scripts/padding-icon") [] "trayerpad" 3600

pacman :: Command
pacman = Com "/bin/sh" ["-c", xBarConfigDir ++ "/utilities/scripts/xmPacman"] "pacman" 600

sound :: Command
sound = Com "/bin/sh" ["-c", xBarConfigDir ++ "/utilities/scripts/xmVolume"] "sound" 10

-------------------------------------------------------------------------------
    -- Monitors

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
        ( " <skyConditionS> <tempC>°C "
        ++ fc (pBorder p) "<rh>% "
        ++ fn 1 "\57982 "
        ++ "<windKmh> km/h <weather>"
        )
    ] 100

dynNet :: Palette -> Monitors
dynNet p = DynNetwork
    ( withHighArgs p
        [ "--template", fn 2 "<txipat><rxipat>"
        , "--suffix", "True"
        , "-d", "1"
        ]
        [ "--rx-icon-pattern", icon "network/rx/network_rx_%%.xpm"
        , "--tx-icon-pattern", icon "network/tx/network_tx_%%.xpm"
        ]
    ) 10

wireless :: Monitors
wireless = Wireless "wlan0"
    [ "-L", "0"
    , "-H", "32"
    , "--normal", "green"
    , "--high", "red"
    ] 10

-------------------------------------------------------------------------------
    -- Others
clock :: Palette -> Date
clock p = Date
    ( action "emacs -e calendar --name 'Orgenda'" 3
        (fc (pLow p) "%T" ++ " - " ++ fc (pBorder p) "%a %e %b %Y")
    ) "date" 10

keyboard :: Kbd
keyboard = Kbd [( "us", "US"), ("it(winkeys)", "IT"), ("de(qwerty)", "DE")]

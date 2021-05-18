module Util
    ( withHighArgs
    , withLowArgs
    , fc
    , fn
    , action
    , icon
    , separator
    ) where



import Colors (Palette(..))



(<~>) :: Palette -> [String] -> [String]
(<~>) p args =
    args ++ ["--low", pLow p, "--normal", pNormal p, "--high", pHigh p]

(>~<) :: Palette -> [String] -> [String]
(>~<) p args =
    args ++ ["--low", pHigh p, "--normal", pNormal p, "--high", pLow p]

withHighArgs :: Palette -> [String] -> [String] -> [String]
withHighArgs p args extras = concat [p <~> args, ["--"], extras]

withLowArgs :: Palette -> [String] -> [String] -> [String]
withLowArgs p args extras = concat [p >~< args, ["--"], extras]

fc :: String -> String -> String
fc color arg = "<fc=" ++ color ++ ">" ++ arg ++ "</fc>"

fn :: Int -> String -> String
fn n arg = "<fn=" ++ show n ++ ">" ++ arg ++ "</fn>"

action :: String -> Int -> String -> String
action com n arg =
    "<action=`" ++ com ++ "` button=" ++ show n ++ ">" ++ arg ++ "</action>"

icon :: String -> String
icon arg = "<icon=" ++ arg ++ "/>"

separator :: String
separator = icon "separators/separator.xpm"

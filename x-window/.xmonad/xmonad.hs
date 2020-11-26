-- xmonad.hs

import Control.Monad
import Data.Monoid
import qualified Data.Map as Map
import Data.Ratio
import Data.Tree
import qualified Graphics.X11.ExtraTypes.XF86 as XF86
import System.IO
import System.Exit

import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import qualified XMonad.Actions.Search as XMSearch
import qualified XMonad.Actions.Submap as XMSubMap
import qualified XMonad.Actions.TreeSelect as XMTS

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Util.SpawnOnce
import XMonad.Util.Cursor

import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Shell

import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Combo
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.ResizableTile
import XMonad.Layout.TwoPanePersistent
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Tabbed
import XMonad.Layout.SimplestFloat

_terminal           = "st"
_modMask            = mod4Mask
_normalBorderColor  = "#151a1e"
_focusedBorderColor = "#FF3333"

_baseWs :: [String]
_baseWs = map show [1..9]

clickable :: [String] -> [String]
clickable ws =
    [  "<action=`xdotool key 0xffeb+"        ++ show key ++ "` button=1>"
    ++ "<action=`xdotool key 0xffeb+0xffe1+" ++ show key ++ "` button=3>"
    ++ nr ++ "</action></action>" | (key,nr) <- zip (map ("0x003"++) ws) ws]

_workspaces :: [String]
_workspaces = clickable _baseWs

_mainFont :: String
_mainFont = "xft:SauceCodePro Nerd Font"

_homeUnix :: String
_homeUnix = "/home/cantoro/"

_selBrowser :: String
_selBrowser = "vivaldi-stable"

_centralBigRect :: W.RationalRect
_centralBigRect = W.RationalRect (1/6) (1/6) (2/3) (2/3)

_centralMedRect :: W.RationalRect
_centralMedRect = W.RationalRect (1/4) (1/4) (1/2) (1/2)

_scratchpads :: [NamedScratchpad]
_scratchpads =
    [ NS "ncmpcpp" cmdNcmpCpp queryNcmpCpp hookNcmpCpp
    , NS "yakuake" cmdYakuake queryYakuake hookYakuake
    , NS "orgenda" cmdOrgenda queryOrgenda hookOrgenda
    ] where
      cmdNcmpCpp   = "st -n ncmpcpp ncmpcpp"
      queryNcmpCpp = resource =? "ncmpcpp"
      hookNcmpCpp  = customFloating _centralMedRect

      cmdYakuake   = "st -n yakuake"
      queryYakuake = resource =? "yakuake"
      hookYakuake  = customFloating _centralBigRect

      cmdOrgenda   = "emacs --name='orgenda' ~/Documents/organization/Notes.org"
      queryOrgenda = resource =? "orgenda"
      hookOrgenda  = customFloating _centralBigRect

_decorationTheme = def
    { fontName            = _mainFont ++ ":pixelsize=8"
    , decoHeight          = 15
    , activeColor         = "#3f4e5a"
    , inactiveColor       = "#151a1e"
    , inactiveBorderColor = _normalBorderColor
    , activeBorderColor   = _focusedBorderColor
    , activeBorderWidth   = 2
    , inactiveBorderWidth = 1
    , activeTextColor     = "#B8CC52"
    , inactiveTextColor   = "#36A3D9" }

_XPromptConfig :: XPConfig
_XPromptConfig = def
    { font                = _mainFont ++ ":style=BoldItalic:size=10:hinting=true"
    , bgColor             = "#151a1e"
    , fgColor             = "#B8CC52"
    , bgHLight            = "#36A3D9"
    , fgHLight            = "#151a1e"
    , borderColor         = "#36A3D9"
    , promptBorderWidth   = 0
    , position            = Top
    , alwaysHighlight     = True
    , maxComplRows        = Just 5
    , historySize         = 256
    , historyFilter       = deleteAllDuplicates
    , promptKeymap        = vimLikeXPKeymap
    , defaultText         = []
    , autoComplete        = Nothing
    , showCompletionOnTab = False
    , searchPredicate     = fuzzyMatch
    , sorter              = fuzzySort
    }

archwiki, reddit, wordreference :: XMSearch.SearchEngine
archwiki      = XMSearch.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
reddit        = XMSearch.searchEngine "reddit" "https://www.reddit.com/search/?q="
wordreference = XMSearch.searchEngine "wordreference" "https://www.wordreference.com/enit/"

_searchEngineMap method = Map.fromList
      [ ((0,         xK_a), method archwiki)
      , ((shiftMask, xK_a), method XMSearch.amazon)
      , ((0,         xK_d), method XMSearch.dictionary)
      , ((shiftMask, xK_d), method XMSearch.duckduckgo)
      , ((0,         xK_g), method XMSearch.google)
      , ((0,         xK_h), method XMSearch.hoogle)
      , ((shiftMask, xK_h), method XMSearch.hackage)
      , ((0,         xK_i), method XMSearch.images)
      , ((0,         xK_m), method XMSearch.maps)
      , ((0,         xK_o), method XMSearch.openstreetmap)
      , ((0,         xK_r), method reddit)
      , ((0,         xK_s), method XMSearch.scholar)
      , ((0,         xK_w), method XMSearch.wikipedia)
      , ((shiftMask, xK_w), method wordreference)
      ]

_treeSelectConfig :: XMTS.TSConfig a
_treeSelectConfig = XMTS.TSConfig
    { XMTS.ts_hidechildren = True
    , XMTS.ts_background   = 0x50c0c0c0
    , XMTS.ts_font         = "xft:mononoki Nerd Font:style=Regular:size=11:hinting=true"
    , XMTS.ts_node         = (0xff00ff00, 0xffffffff)
    , XMTS.ts_nodealt      = (0x8000ff00, 0xffffffff)
    , XMTS.ts_highlight    = (0xffffffff, 0xff755999)
    , XMTS.ts_extra        = 0xff000000
    , XMTS.ts_node_width   = 200
    , XMTS.ts_node_height  = 20
    , XMTS.ts_originX      = 0
    , XMTS.ts_originY      = 0
    , XMTS.ts_indent       = 50
    , XMTS.ts_navigate     = _treeSelectNavigation
    }

_treeSelectNavigation = Map.fromList
    [ ((0, xK_Escape),      XMTS.cancel)
    , ((0, xK_Return),      XMTS.select)
    , ((0, xK_space),       XMTS.select)
    , ((0, xK_Up),          XMTS.movePrev)
    , ((0, xK_Down),        XMTS.moveNext)
    , ((0, xK_Left),        XMTS.moveParent)
    , ((0, xK_Right),       XMTS.moveChild)
    , ((0, xK_k),           XMTS.movePrev)
    , ((0, xK_j),           XMTS.moveNext)
    , ((0, xK_h),           XMTS.moveParent)
    , ((0, xK_l),           XMTS.moveChild)
    , ((controlMask, xK_p), XMTS.moveHistBack)
    , ((controlMask, xK_n), XMTS.moveHistForward)
    -- , ((0, xK_a),        XMTS.moveTo ["+ Accessories"])
    ]

_treeSelecAction :: XMTS.TSConfig (X ()) -> X ()
_treeSelecAction a = XMTS.treeselectAction a
    [ Node (XMTS.TSNode "\62211 Menu" "Most used desktop applications" (return ()))
        [ Node (XMTS.TSNode "\63184 Baobab"             "A graphical tool to anlyze disk usage"
            (spawn "baobab")) []
        , Node (XMTS.TSNode "\62098 EasyTAG"            "View and edit tags in audio files"
            (spawn "easytag")) []
        , Node (XMTS.TSNode "\59191 Emacs"              "The hackers text editor"
            (spawn "emacs")) []
        , Node (XMTS.TSNode "\62622 Evince"             "A simple document viewer for GNOME"
            (spawn "evince")) []
        , Node (XMTS.TSNode "\63433 Gnome Disks"        "View, modify and configure disks and media"
            (spawn "gnome-disks")) []
        , Node (XMTS.TSNode "\62979 Gpick"              "Advanced color picker"
            (spawn "gpick")) []
        , Node (XMTS.TSNode "冀 ParaView"               "Open-source, multi-platform data analysis and visualization application"
            (spawn "paraview")) []
        , Node (XMTS.TSNode "\62500 RadioTray"          "Radio for system trayer"
            (spawn "radiotray-ng")) []
        , Node (XMTS.TSNode "禍 SUSE Image Writer"      "OpenSUSE image writer"
            (spawn "imagewriter")) []
        , Node (XMTS.TSNode "\61465 Transmission"       "Bittorrent client"
            (spawn "transmission-gtk")) []
        , Node (XMTS.TSNode "\61564 Thunar"             "Xfce File Manager"
            (spawn "thunar")) []
        , Node (XMTS.TSNode "\57953 XBoard"             "X chess board"
            (spawn ("xboard -fcp '" ++ _homeUnix ++ "Documents/chess/engines/stockfish12_modern' -fd '" ++ _homeUnix ++ "Documents/chess/engines' -fn 'Stockfish12' -fUCI"))) []
        , Node (XMTS.TSNode "\62600 Vivaldi"            "A Browser For Our Friends"
            (spawn "vivaldi-stable")) []
        , Node (XMTS.TSNode "\63685 WPS Presentation"   "Simplicity Creates Marvels"
            (spawn "wpp")) []
        , Node (XMTS.TSNode "\63685 WPS Pdf"            "Simplicity Creates Marvels"
            (spawn "wpspdf")) []
        , Node (XMTS.TSNode "\63685 WPS Spreadsheets"   "Simplicity Creates Marvels"
            (spawn "et")) []
        , Node (XMTS.TSNode "\63685 WPS Writer"         "Simplicity Creates Marvels"
            (spawn "wps")) []
        ]
    , Node (XMTS.TSNode "Shell Scripts" "Useful or commont scripts" (return ()))
        [ Node (XMTS.TSNode "Manual Printing" "dmenu and man together"
            (spawn "demu_man")) []
        , Node (XMTS.TSNode "Dmenu Todo" "fast todo list"
            (spawn "demu_todo")) []
        , Node (XMTS.TSNode "Book search" "Browse all my pdf"
            (spawn "book-search")) []
        , Node (XMTS.TSNode "Web search" "Web searching through rofi"
            (spawn "web-search")) []
        ]
    , Node (XMTS.TSNode "\62911 Bookmarks" "a list of web bookmarks" (return ()))
        [ Node (XMTS.TSNode "Linux" "a list of web bookmarks" (return ()))
            [ Node (XMTS.TSNode "Arch Linux" "btw, i use arch!" (return ()))
                [ Node (XMTS.TSNode "Arch Linux"   "Arch Linux homepage"
                    (spawn (_selBrowser ++ " https://www.archlinux.org/"))) []
                , Node (XMTS.TSNode "Arch Wiki"    "The best Linux wiki"
                    (spawn (_selBrowser ++ " https://wiki.archlinux.org/"))) []
                , Node (XMTS.TSNode "AUR"          "Arch User Repository"
                    (spawn (_selBrowser ++ " https://aur.archlinux.org/"))) []
                , Node (XMTS.TSNode "Arch Forums"  "Arch Linux web forum"
                    (spawn (_selBrowser ++ " https://bbs.archlinux.org/"))) []
                ]
            , Node (XMTS.TSNode "XMonad" "xmonad documentation" (return ()))
                [ Node (XMTS.TSNode "XMonad"                "Homepage for XMonad"
                    (spawn (_selBrowser ++ " http://xmonad.org"))) []
                , Node (XMTS.TSNode "XMonad GitHub"         "The GitHub page for XMonad"
                    (spawn (_selBrowser ++ " https://github.com/xmonad/xmonad"))) []
                , Node (XMTS.TSNode "xmonad-contrib"        "Third party extensions for XMonad"
                    (spawn (_selBrowser ++ " https://hackage.haskell.org/package/xmonad-contrib"))) []
                , Node (XMTS.TSNode "xmonad-contrib GitHub" "The GitHub page for xmonad-contrib"
                    (spawn (_selBrowser ++ " https://github.com/xmonad/xmonad-contrib"))) []
                , Node (XMTS.TSNode "Xmobar"                 "Minimal text-based status bar"
                    (spawn (_selBrowser ++ " https://hackage.haskell.org/package/xmobar"))) []
                ]
            , Node (XMTS.TSNode "Emacs" "Emacs documentation" (return ()))
                [ Node (XMTS.TSNode "GNU Emacs" "Extensible free/libre text editor"
                    (spawn (_selBrowser ++ " https://www.gnu.org/software/emacs/"))) []
                , Node (XMTS.TSNode "Doom Emacs" "Emacs distribution with sane defaults"
                    (spawn (_selBrowser ++ " https://github.com/hlissner/doom-emacs"))) []
                , Node (XMTS.TSNode "r/emacs" "M-x emacs-reddit"
                    (spawn (_selBrowser ++ " https://www.reddit.com/r/emacs/"))) []
                , Node (XMTS.TSNode "EmacsWiki" "EmacsWiki Site Map"
                    (spawn (_selBrowser ++ " https://www.emacswiki.org/emacs/SiteMap"))) []
                , Node (XMTS.TSNode "Emacs StackExchange" "Q&A site for emacs"
                    (spawn (_selBrowser ++ " https://emacs.stackexchange.com/"))) []
                , Node (XMTS.TSNode "Emacs Lisp" "Reference manual for elisp"
                    (spawn (_selBrowser ++ " https://www.gnu.org/software/emacs/manual/html_node/elisp/"))) []
                , Node (XMTS.TSNode "Learn Elisp in Y Minutes" "Single webpage for elisp basics"
                    (spawn (_selBrowser ++ " https://learnxinyminutes.com/docs/elisp/"))) []
                , Node (XMTS.TSNode "r/Lisp" "Subreddit for lisp languages"
                    (spawn (_selBrowser ++ " https://www.reddit.com/r/lisp/"))) []
                ]
            ]
        , Node (XMTS.TSNode "Haskell" "haskell documentation" (return ()))
            [ Node (XMTS.TSNode "Haskell.org" "Homepage for haskell"
                (spawn (_selBrowser ++ " http://www.haskell.org"))) []
            , Node (XMTS.TSNode "Hoogle" "Haskell API search engine"
                (spawn (_selBrowser ++ " https://hoogle.haskell.org/"))) []
            , Node (XMTS.TSNode "r/haskell" "Subreddit for haskell"
                (spawn (_selBrowser ++ " https://www.reddit.com/r/Python/"))) []
            , Node (XMTS.TSNode "Haskell on StackExchange" "Newest haskell topics on StackExchange"
                (spawn (_selBrowser ++ " https://stackoverflow.com/questions/tagged/haskell"))) []
            ]
        , Node (XMTS.TSNode "Python" "python documentation" (return ()))
            [ Node (XMTS.TSNode "Python.org" "Homepage for python"
                (spawn (_selBrowser ++ " https://www.python.org/"))) []
            , Node (XMTS.TSNode "r/Python" "Subreddit for python"
                (spawn (_selBrowser ++ " https://www.reddit.com/r/Python/"))) []
            , Node (XMTS.TSNode "Python on StackExchange" "Newest python topics on StackExchange"
                (spawn (_selBrowser ++ " https://stackoverflow.com/questions/tagged/python"))) []
            ]
        ]
    , Node (XMTS.TSNode "\59333 DotFiles" "a list of configuration files" (return ()))
        [ Node (XMTS.TSNode "Neovim" "Neovim config file"
            (spawn ("st nvim " ++ _homeUnix ++ ".config/nvim/init.vim"))) []
        , Node (XMTS.TSNode "XMonad" "Xmonad config file"
            (spawn ("st nvim " ++ _homeUnix ++ ".xmonad/xmonad.hs"))) []
        ]
    , Node (XMTS.TSNode "Multimedia Control" "mpc and mpv" (return ()))
        [ Node (XMTS.TSNode "Mpc Prev" ""
            (spawn "mpc prev")) []
        , Node (XMTS.TSNode "Mpc Next" ""
            (spawn "mpc next")) []
        , Node (XMTS.TSNode "Mpc Play" ""
            (spawn "mpc play")) []
        , Node (XMTS.TSNode "Mpc Pause" ""
            (spawn "mpc pause")) []
        , Node (XMTS.TSNode "Mpv Start/Pause All" ""
            (spawn "mpv_bulk_toggle")) []
        , Node (XMTS.TSNode "Mpc Quit All" ""
            (spawn "mpv_bulk_quit")) []
        ]
    , Node (XMTS.TSNode "Brightness" "Sets screen brightness using xbacklight" (return ()))
        [ Node (XMTS.TSNode "Bright" "FULL POWER!!"            (spawn "xbacklight -set 100")) []
        , Node (XMTS.TSNode "Normal" "Normal Brightness (50%)" (spawn "xbacklight -set 50"))  []
        , Node (XMTS.TSNode "Dim"    "Quite dark"              (spawn "xbacklight -set 10"))  []
        ]
    , Node (XMTS.TSNode "Shutdown" "shutdown, reboot, logout, lock" (return ()))
        [ Node (XMTS.TSNode "Shutdown" "Bye,Bye"
            (spawn "shutdown now")) []
        , Node  (XMTS.TSNode "Reboot" "See ya!"
            (spawn "reboot")) []
        , Node  (XMTS.TSNode "LogOut" "Buttlicker"
            (io exitSuccess)) []
        , Node  (XMTS.TSNode "Lock" "Unchain my heart"
            (spawn "slock")) []
        ]
    , Node (XMTS.TSNode "\63237 Exit" "" (spawn "xdotool key Escape")) []
    ]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

_topXmobarPP h = xmobarPP
    { ppCurrent = xmobarColor "#B8CC52" "" . wrap "" ""
    , ppVisible = xmobarColor "#36A3D9" "" . wrap "" ""
    , ppHidden  = xmobarColor "#465764" "" . wrap "" ""
    , ppUrgent  = xmobarColor "#FF3333" "" . wrap "" ""
    , ppSep     = " "
    , ppTitle   = xmobarColor "#B8CC52" "#232b32" . shorten 35
    , ppLayout  = xmobarColor "#36A3D9" "#232b32"
    , ppOutput  = hPutStrLn h
    , ppExtras  = [windowCount]
    , ppOrder   = \(ws:l:t:ex) -> ex ++ [l,ws,t]
    }

_keys conf@XConfig {XMonad.modMask = modm} = Map.fromList $
    [ ((modm,                 xK_q), spawn "xmonad --recompile; xmonad --restart")
    , ((modm .|. shiftMask,   xK_q), kill)
    , ((modm,                 xK_t), withFocused $ windows . W.sink)
    , ((modm .|. shiftMask,   xK_t), withFocused $ windows . flip W.float _centralBigRect)
    , ((modm,                 xK_a), _treeSelecAction _treeSelectConfig)

    , ((modm,                 xK_s), incWindowSpacing 1)
    , ((modm .|. shiftMask,   xK_s), decWindowSpacing 1)
    , ((modm .|. controlMask, xK_s), setScreenWindowSpacing 0)

    , ((modm,                 xK_f), spawn _selBrowser)
    , ((modm .|. shiftMask,   xK_f), refresh)

    , ((modm,                 xK_b), sendMessage ToggleStruts)

    , ((modm .|. controlMask, xK_m), namedScratchpadAction _scratchpads "ncmpcpp")
    , ((modm .|. controlMask, xK_y), namedScratchpadAction _scratchpads "yakuake")
    , ((modm .|. controlMask, xK_o), namedScratchpadAction _scratchpads "orgenda")

    , ((modm,                 xK_p), XMSubMap.submap . Map.fromList $
        [ ((0, xK_Left ), spawn "mpc prev")
        , ((0, xK_Right), spawn "mpc next")
        , ((0, xK_Up   ), spawn "mpc play")
        , ((0, xK_Down ), spawn "mpc pause")
        , ((0, xK_space), spawn "mpc toggle")
        , ((0, xK_t    ), spawn "mpv_bulk_toggle")
        , ((0, xK_q    ), spawn "mpv_bulk_quit")
        ])

    , ((modm,                 xK_u), spawn "rofi -modi drun,run -show drun")
    , ((modm .|. shiftMask,   xK_u), spawn "dmenu_run_timed")
    , ((modm .|. controlMask, xK_u), spawn "xmenu-apps")

    , ((modm,                               xK_h), sendMessage Shrink)
    , ((modm,                               xK_l), sendMessage Expand)
    , ((modm,                               xK_j), windows W.focusDown)
    , ((modm,                               xK_k), windows W.focusUp)
    , ((modm .|. shiftMask,                 xK_h), sendMessage MirrorShrink)
    , ((modm .|. shiftMask,                 xK_l), sendMessage MirrorExpand)
    , ((modm .|. shiftMask,                 xK_j), windows W.swapDown)
    , ((modm .|. shiftMask,                 xK_k), windows W.swapUp)
    , ((modm .|. controlMask,               xK_h), sendMessage $ Go L)
    , ((modm .|. controlMask,               xK_l), sendMessage $ Go R)
    , ((modm .|. controlMask,               xK_j), sendMessage $ Go D)
    , ((modm .|. controlMask,               xK_k), sendMessage $ Go U)
    , ((modm .|. mod1Mask,                  xK_h), sendMessage $ Swap L)
    , ((modm .|. mod1Mask,                  xK_l), sendMessage $ Swap R)
    , ((modm .|. mod1Mask,                  xK_j), sendMessage $ Swap D)
    , ((modm .|. mod1Mask,                  xK_k), sendMessage $ Swap U)
    , ((modm .|. controlMask .|. shiftMask, xK_h), sendMessage $ Move L)
    , ((modm .|. controlMask .|. shiftMask, xK_l), sendMessage $ Move R)
    , ((modm .|. controlMask .|. shiftMask, xK_j), sendMessage $ Move D)
    , ((modm .|. controlMask .|. shiftMask, xK_k), sendMessage $ Move U)

    , ((modm,                               xK_m), windows W.focusMaster)
    , ((modm .|. shiftMask,                 xK_m), windows W.swapMaster)

    , ((modm,                 xK_comma    ), sendMessage (IncMasterN 1))
    , ((modm,                 xK_period   ), sendMessage (IncMasterN (-1)))

    , ((modm,                 xK_bracketleft),  prevWS)
    , ((modm,                 xK_bracketright), nextWS)
    , ((modm .|. shiftMask,   xK_bracketleft),  shiftToPrev >> prevWS)
    , ((modm .|. shiftMask,   xK_bracketright), shiftToNext >> nextWS)

    , ((modm,                 xK_Tab      ), sendMessage NextLayout)
    , ((modm .|. shiftMask,   xK_Tab      ), setLayout $ XMonad.layoutHook conf)

    , ((modm,                 xK_space    ), sendMessage $ JumpToLayout "Monocle")
    , ((modm .|. shiftMask,   xK_space    ), sendMessage $ JumpToLayout "Float")

    , ((modm .|. shiftMask,   xK_Delete   ), io exitSuccess)
    , ((modm .|. shiftMask,   xK_BackSpace), spawn "slock")

    , ((modm,                 xK_Return   ), spawn "alacritty")
    , ((modm .|. shiftMask,   xK_Return   ), spawn $ XMonad.terminal conf)

    , ((0,                    xK_Print    ), spawn "scrot ~/Pictures/Screenshots/%Y-%m-%d-%T-screenshoot.png; notify-send -i photo 'Taken fullscreen screenshot'")
    , ((0    .|. shiftMask,   xK_Print    ), spawn "import ~/Pictures/Screenshots/$(date +%Y-%m-%d-%T)screenshoot.png; notify-send -i photo 'Saved to ~/Pictures/Screenshots'")

    , ((modm,               xK_F1 ), manPrompt   _XPromptConfig)
    , ((modm,               xK_F5 ), shellPrompt _XPromptConfig)
    , ((modm,               xK_F2 ), XMSubMap.submap $ _searchEngineMap $ XMSearch.promptSearch _XPromptConfig)
    , ((modm .|. shiftMask, xK_F2 ), XMSubMap.submap $ _searchEngineMap  XMSearch.selectSearch)

    , ((0, XF86.xF86XK_AudioMute),         spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ((0, XF86.xF86XK_AudioLowerVolume),  spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ((0, XF86.xF86XK_AudioRaiseVolume),  spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")

    , ((0, XF86.xF86XK_MonBrightnessUp),   spawn "xbacklight -inc 5")
    , ((0, XF86.xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
    , ((0, XF86.xF86XK_Display),           spawn "monitor_handler")
    -- XF86Tools
    -- XF86Search
    -- XF86LaunchA
    -- XF86Explorer
    ]
    ++
    -- Switch/MoveClient to workspace N --> mod(+Shift)-[1..9]
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- Switch/MoveClient to physical/Xinerama screens 1, 2, or 3 --> mod(+Shift)-{w,e,r}
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

_mouseBindings XConfig {XMonad.modMask = modm} = Map.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                      >> windows W.shiftMaster)
    ]

_manageHook = composeAll $
    [ className =? _classToFloat --> doCenterFloat | _classToFloat <- _toFloat ]
    ++
    [ (className =? "Display" <&&> title =? "ImageMagick: ") --> doCenterFloat

    , title      =? "Event Tester"                           --> doFloat
    , title      =? "lstopo"                                 --> doCenterFloat
    , title      =? "weatherreport"                          --> doRectFloat _centralBigRect

    , _role      =? "ncmpcpp"                                --> doCenterFloat
    , _role      =? "pop-up"                                 --> doCenterFloat

    , (className =? "Thunar" <&&>
        title =? "Bulk Rename - Rename Multiple Files")      --> doCenterFloat
    , (className =? "Thunar" <&&>
        title =? "File Operation Progress")                  --> doCenterFloat
    -- doShift
    , className =? "Transmission-gtk" --> doShift      (_workspaces !! 8)
    , className =? "mpv"              --> doShiftAndGo (_workspaces !! 4)
    -- doIgnore
    , resource   =? "stalonetray" --> doIgnore
    -- , resource   =? "desktop_window" --> doIgnore
    -- , resource   =? "kdesktop"       --> doIgnore
    -- ScratchPads
    , namedScratchpadManageHook _scratchpads
    ]
  where _toFloat = [ "Arandr"
                    , "Avahi-discover"
                    , "Baobab"
                    , "Blueberry.py"
                    , "Bssh"
                    , "Bvnc"
                    , "CMakeSetup"
                    , "Exo-helper-2"
                    , "feh"
                    , "Gimp"
                    , "Gnome-disks"
                    , "Gpick"
                    , "Hardinfo"
                    , "imagewriter"
                    , "Lightdm-gtk-greeter-settings"
                    , "Lxappearance"
                    , "MPlayer"
                    , "Nitrogen"
                    , "Pavucontrol"
                    , "qv4l2"
                    , "qvidcap"
                    , "Sxiv"
                    , "System-config-printer.py"
                    , "Transmission-gtk"
                    , "Xarchiver"
                    , "Xboard"
                    , "Xfce4-about"
                    , "Xmessage"
                    , "Yad"
                    , "Yad-icon-browser"
                    ]
        doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
        _role = stringProperty "WM_WINDOW_ROLE"
        _name = stringProperty "WM_NAME"

_handleEventHook = mempty

_startupHook = do
    startupHook def
    setDefaultCursor xC_left_ptr

-- Layouts
_applySpacing sz = spacingRaw True (Border sz sz sz sz) True (Border sz sz sz sz) True

tall       = renamed [Replace "Tall"] $
                ResizableTall 1 0.03 0.5 []
combo      = renamed [Replace "Combo"] (
                combineTwo (Tall 1 0.03 0.5)
                    (tabbed shrinkText _decorationTheme)
                    (tabbed shrinkText _decorationTheme))
monocle    = renamed [Replace "Monocle"] $
                noBorders $
                tabbed shrinkText _decorationTheme
twoPane    = renamed [Replace "TwoPane"] $
                TwoPanePersistent Nothing 0.03 0.5
horizontal = renamed [Replace "Horizontal"] $
                Mirror (ResizableTall 1 0.03 0.5 [])
treeCol    = ThreeColMid 1 0.03 0.5
floatL     = renamed [Replace "Float"]
                simplestFloat

_layouts =
    tall
    ||| combo
    ||| monocle
    ||| twoPane
    ||| horizontal
    ||| treeCol
    ||| floatL

_layoutHook =
    avoidStrutsOn [U] $ smartBorders $ windowNavigation $
    renamed [CutWordsLeft 1] $ _applySpacing 0 _layouts

main :: IO ()
main = do
  _topXmobar <- spawnPipe "xmobar"
  xmonad $ docks $ ewmh def
    { terminal           = _terminal
    , modMask            = mod4Mask
    , focusFollowsMouse  = False
    , clickJustFocuses   = True
    , workspaces         = _workspaces
    , borderWidth        = 1
    , normalBorderColor  = _normalBorderColor
    , focusedBorderColor = _focusedBorderColor
    , keys               = _keys
    , mouseBindings      = _mouseBindings
    , manageHook         = _manageHook <+> manageDocks
    , handleEventHook    = _handleEventHook <+> ewmhDesktopsEventHook
    , logHook            = dynamicLogWithPP $ _topXmobarPP _topXmobar
    , startupHook        = _startupHook
    , layoutHook         = _layoutHook
    }

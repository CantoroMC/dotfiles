{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module XMonad.Local.Bindings.Util
    ( Direction (..)
    , moveFloating
    , resizeFloating
    , xmPromptConfig
    , xmSearchEngineMap
    , xmTreeSelectAction
    , xmTreeSelectConfig
    , terminalFromConf
    , inTerminalFromConf
    ) where

import qualified Data.Map as Map
    ( Map
    , fromList
    )
import Data.Tree
    ( Tree (..)
    )
import System.Exit
    ( exitSuccess
    )

import XMonad

import XMonad.Actions.FloatKeys
    ( keysMoveWindow
    , keysResizeWindow
    )

import XMonad.Prompt
    ( XPConfig (..)
    , XPPosition (..)
    , vimLikeXPKeymap
    , deleteAllDuplicates
    )
import XMonad.Prompt.FuzzyMatch
    ( fuzzyMatch
    , fuzzySort
    )
import qualified XMonad.Actions.Search as XMSearch
    ( SearchEngine (..)
    , searchEngine
    , amazon
    , dictionary
    , duckduckgo
    , google
    , hoogle
    , hackage
    , images
    , maps
    , openstreetmap
    , scholar
    , wikipedia
    )
import qualified XMonad.Actions.TreeSelect as XMTS
    ( TSConfig (..)
    , TSNode (..)
    , treeselectAction
    , cancel
    , select
    , movePrev
    , moveNext
    , moveParent
    , moveChild
    , moveHistBack
    , moveHistForward
    -- , moveTo
    )

import qualified XMonad.Local.Config.Theme as XMTheme

------------------------------------------------------------------------------
    -- User
data Direction = FL
               | FD
               | FU
               | FR

moveFloating :: Direction -> Window -> X ()
moveFloating d = keysMoveWindow (direction d)

resizeFloating :: Direction -> Window -> X ()
resizeFloating d = keysResizeWindow (direction d) (0 , 0)

direction :: Direction -> D
direction d = (dx , dy)
  where (dx , dy) = case d of FL -> (-pixel , 0)
                              FD -> (0 , pixel)
                              FU -> (0 , -pixel)
                              FR -> (pixel , 0)
        pixel = 20

terminalFromConf :: (MonadIO m, MonadReader XConf m) => m String
terminalFromConf = reader $ terminal . config

inTerminalFromConf :: (MonadIO m, MonadReader XConf m) => String -> m String
inTerminalFromConf prog = do terminalEmulator <- terminalFromConf
                             return $ terminalEmulator <> " -t " <> prog <> " " <> prog
------------------------------------------------------------------------------
    -- Prompt, Search Engine and Tree Select
-- XMonad Prompt Configuration
xmPromptConfig :: XPConfig
xmPromptConfig = def
    { font                = "xft:Operator Mono Lig:style=BoldItalic:size=10:hinting=true"
    , bgColor             = XMTheme.inactiveColor XMTheme.xmTheme
    , fgColor             = XMTheme.activeTextColor XMTheme.xmTheme
    , bgHLight            = XMTheme.inactiveTextColor XMTheme.xmTheme
    , fgHLight            = XMTheme.inactiveColor XMTheme.xmTheme
    , borderColor         = XMTheme.urgentBorderColor XMTheme.xmTheme
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

-- SearchEngine Configuration
archwiki, reddit, wordreference :: XMSearch.SearchEngine
archwiki      = XMSearch.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
reddit        = XMSearch.searchEngine "reddit" "https://www.reddit.com/search/?q="
wordreference = XMSearch.searchEngine "wordreference" "https://www.wordreference.com/enit/"

xmSearchEngineMap :: (XMSearch.SearchEngine -> a) -> Map.Map (KeyMask, KeySym) a
xmSearchEngineMap method = Map.fromList
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

-- TreeSelect Configuration
xmBrowser :: String
xmBrowser = "vivaldi-stable"

xmTreeSelectConfig :: XMTS.TSConfig a
xmTreeSelectConfig = XMTS.TSConfig
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
    , XMTS.ts_navigate     = xmTreeSelectNavigation
    }

xmTreeSelectNavigation = Map.fromList
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

xmTreeSelectAction :: XMTS.TSConfig (X ()) -> X ()
xmTreeSelectAction a = XMTS.treeselectAction a
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
        , Node (XMTS.TSNode "\61564 Nautilus"            "Gnome File Manager"
            (spawn "nautilus")) []
        , Node (XMTS.TSNode "\57953 XBoard"             "X chess board"
            (spawn "xboard -fcp '\"${HOME}\"/Documents/chess/engines/stockfish12_modern' -fd '\"${HOME}\"/Documents/chess/engines' -fn 'Stockfish12' -fUCI")) []
        , Node (XMTS.TSNode "\62600 Vivaldi"            "A Browser For Our Friends"
            (spawn "vivaldi-stable")) []
        ]
    , Node (XMTS.TSNode "Shell Scripts" "Useful or commont scripts" (return ()))
        [ Node (XMTS.TSNode "Manual Printing" "dmenu and man together"
            (spawn "dmenu_man")) []
        , Node (XMTS.TSNode "Dmenu Todo" "fast todo list"
            (spawn "dmenu_todo")) []
        , Node (XMTS.TSNode "Book search" "Browse all my pdf"
            (spawn "book-search")) []
        , Node (XMTS.TSNode "Web search" "Web searching through rofi"
            (spawn "web-search")) []
        ]
    , Node (XMTS.TSNode "\62911 Bookmarks" "a list of web bookmarks" (return ()))
        [ Node (XMTS.TSNode "Linux" "a list of web bookmarks" (return ()))
            [ Node (XMTS.TSNode "Arch Linux" "btw, i use arch!" (return ()))
                [ Node (XMTS.TSNode "Arch Linux"   "Arch Linux homepage"
                    (spawn (xmBrowser ++ " https://www.archlinux.org/"))) []
                , Node (XMTS.TSNode "Arch Wiki"    "The best Linux wiki"
                    (spawn (xmBrowser ++ " https://wiki.archlinux.org/"))) []
                , Node (XMTS.TSNode "AUR"          "Arch User Repository"
                    (spawn (xmBrowser ++ " https://aur.archlinux.org/"))) []
                , Node (XMTS.TSNode "Arch Forums"  "Arch Linux web forum"
                    (spawn (xmBrowser ++ " https://bbs.archlinux.org/"))) []
                ]
            , Node (XMTS.TSNode "XMonad" "xmonad documentation" (return ()))
                [ Node (XMTS.TSNode "XMonad"                "Homepage for XMonad"
                    (spawn (xmBrowser ++ " http://xmonad.org"))) []
                , Node (XMTS.TSNode "XMonad GitHub"         "The GitHub page for XMonad"
                    (spawn (xmBrowser ++ " https://github.com/xmonad/xmonad"))) []
                , Node (XMTS.TSNode "xmonad-contrib"        "Third party extensions for XMonad"
                    (spawn (xmBrowser ++ " https://hackage.haskell.org/package/xmonad-contrib"))) []
                , Node (XMTS.TSNode "xmonad-contrib GitHub" "The GitHub page for xmonad-contrib"
                    (spawn (xmBrowser ++ " https://github.com/xmonad/xmonad-contrib"))) []
                , Node (XMTS.TSNode "Xmobar"                 "Minimal text-based status bar"
                    (spawn (xmBrowser ++ " https://hackage.haskell.org/package/xmobar"))) []
                ]
            , Node (XMTS.TSNode "Emacs" "Emacs documentation" (return ()))
                [ Node (XMTS.TSNode "GNU Emacs" "Extensible free/libre text editor"
                    (spawn (xmBrowser ++ " https://www.gnu.org/software/emacs/"))) []
                , Node (XMTS.TSNode "Doom Emacs" "Emacs distribution with sane defaults"
                    (spawn (xmBrowser ++ " https://github.com/hlissner/doom-emacs"))) []
                , Node (XMTS.TSNode "r/emacs" "M-x emacs-reddit"
                    (spawn (xmBrowser ++ " https://www.reddit.com/r/emacs/"))) []
                , Node (XMTS.TSNode "EmacsWiki" "EmacsWiki Site Map"
                    (spawn (xmBrowser ++ " https://www.emacswiki.org/emacs/SiteMap"))) []
                , Node (XMTS.TSNode "Emacs StackExchange" "Q&A site for emacs"
                    (spawn (xmBrowser ++ " https://emacs.stackexchange.com/"))) []
                , Node (XMTS.TSNode "Emacs Lisp" "Reference manual for elisp"
                    (spawn (xmBrowser ++ " https://www.gnu.org/software/emacs/manual/html_node/elisp/"))) []
                , Node (XMTS.TSNode "Learn Elisp in Y Minutes" "Single webpage for elisp basics"
                    (spawn (xmBrowser ++ " https://learnxinyminutes.com/docs/elisp/"))) []
                , Node (XMTS.TSNode "r/Lisp" "Subreddit for lisp languages"
                    (spawn (xmBrowser ++ " https://www.reddit.com/r/lisp/"))) []
                ]
            ]
        , Node (XMTS.TSNode "Haskell" "haskell documentation" (return ()))
            [ Node (XMTS.TSNode "Haskell.org" "Homepage for haskell"
                (spawn (xmBrowser ++ " http://www.haskell.org"))) []
            , Node (XMTS.TSNode "Hoogle" "Haskell API search engine"
                (spawn (xmBrowser ++ " https://hoogle.haskell.org/"))) []
            , Node (XMTS.TSNode "r/haskell" "Subreddit for haskell"
                (spawn (xmBrowser ++ " https://www.reddit.com/r/Python/"))) []
            , Node (XMTS.TSNode "Haskell on StackExchange" "Newest haskell topics on StackExchange"
                (spawn (xmBrowser ++ " https://stackoverflow.com/questions/tagged/haskell"))) []
            ]
        , Node (XMTS.TSNode "Python" "python documentation" (return ()))
            [ Node (XMTS.TSNode "Python.org" "Homepage for python"
                (spawn (xmBrowser ++ " https://www.python.org/"))) []
            , Node (XMTS.TSNode "r/Python" "Subreddit for python"
                (spawn (xmBrowser ++ " https://www.reddit.com/r/Python/"))) []
            , Node (XMTS.TSNode "Python on StackExchange" "Newest python topics on StackExchange"
                (spawn (xmBrowser ++ " https://stackoverflow.com/questions/tagged/python"))) []
            ]
        ]
    , Node (XMTS.TSNode "\59333 DotFiles" "a list of configuration files" (return ()))
        [ Node (XMTS.TSNode "Neovim" "Neovim config file"
            (spawn "st nvim \"${XDG_CONFIG_HOME}\"/nvim/init.vim")) []
        , Node (XMTS.TSNode "XMonad" "Xmonad config file"
            (spawn "st nvim \"${XDG_CONFIG_HOME}\"/xmonad/xmonad.hs")) []
        , Node (XMTS.TSNode "XMobar" "Xmobar config file"
            (spawn "st nvim \"${XDG_CONFIG_HOME}\"/xmobar/xmobar.hs")) []
        , Node (XMTS.TSNode "Xprofile" "Startup applications and settings"
            (spawn "st nvim \"${XDG_CONFIG_HOME}\"/X11/xprofile")) []
        , Node (XMTS.TSNode ".zlogin" "Zshell login script"
            (spawn "st nvim \"${XDG_CONFIG_HOME}\"/.config/zsh/.zlogin")) []
        , Node (XMTS.TSNode "Shell Environment" "Environmental variables and path"
            (spawn "st nvim \"${XDG_CONFIG_HOME}\"/.zshenv")) []
        , Node (XMTS.TSNode "Zsh Configuration" "ZShell configuration (uses OMZ)"
            (spawn "st nvim \"${XDG_CONFIG_HOME}\"/zsh/.zshrc")) []
        , Node (XMTS.TSNode "Shell Alias" "My Shell aliases plugged into zsh"
            (spawn "st nvim \"${XDG_CONFIG_HOME}\"/zsh/lib/my_aliases.zsh")) []
        , Node (XMTS.TSNode "Shell Functions" "My Shell functions plugged into zsh"
            (spawn "st nvim \"${XDG_CONFIG_HOME}\"/zsh/lib/my_functions.zsh")) []
        , Node (XMTS.TSNode "ZShell Keybindings" "Zsh Vi keybindins and zle"
            (spawn "st nvim \"${XDG_CONFIG_HOME}\"/zsh/lib/key-bindings.zsh")) []
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
            (spawn "i3lock -i ~/.config/xmonad/screenlocker.png -t -f -e")) []
        ]
    , Node (XMTS.TSNode "\63237 Exit" "" (spawn "xdotool key Escape")) []
    ]

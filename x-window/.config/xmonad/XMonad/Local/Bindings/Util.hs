{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module XMonad.Local.Bindings.Util
    ( Direction (..)
    , moveFloating
    , resizeFloating
    , xmPromptConfig
    , xmSearchEngineMap
    , terminalFromConf
    , inTerminalFromConf
    ) where

import qualified Data.Map as Map
    ( Map
    , fromList
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
    -- Prompt and Search Engine
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

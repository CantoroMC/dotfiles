{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-------------------------------------------------------------------------------
    -- Imports
module Layout.Util
    ( applySpacing
    , xmLayouts
    ) where

import XMonad hiding ((|||))

import XMonad.Layout.Combo (CombineTwo, combineTwo)
import XMonad.Layout.Decoration (Decoration, DefaultShrinker, Theme(..))

import XMonad.Layout.IfMax (IfMax(..))

import XMonad.Layout.LayoutCombinators ((|||))
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Renamed (Rename(CutWordsLeft, Replace), renamed)
import XMonad.Layout.ResizableThreeColumns
    (ResizableThreeCol(ResizableThreeColMid))
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.Simplest (Simplest(..))
import XMonad.Layout.SimplestFloat (SimplestFloat, simplestFloat)
import XMonad.Layout.Spacing (Border(..), Spacing(..), spacingRaw)
import XMonad.Layout.Tabbed (TabbedDecoration(..), shrinkText, tabbed)

import XMonad.Layout.WindowArranger (WindowArranger)

import qualified Config.Theme as XMTheme
import Config.Workspace (xmWorkspaces)
import Layout.Tatami (Tatami(..))

-------------------------------------------------------------------------------
    -- Decorations Theme

xmDecorationTheme :: Theme
xmDecorationTheme = def
    { fontName            = "xft:Operator Mono Lig:pixelsize=11"
    , decoHeight          = 15
    , activeColor         = XMTheme.activeColor XMTheme.xmTheme
    , inactiveColor       = XMTheme.inactiveColor XMTheme.xmTheme
    , inactiveBorderColor = XMTheme.inactiveBorderColor XMTheme.xmTheme
    , activeBorderColor   = XMTheme.activeBorderColor XMTheme.xmTheme
    , activeBorderWidth   = 1
    , inactiveBorderWidth = 1
    , activeTextColor     = XMTheme.activeTextColor XMTheme.xmTheme
    , inactiveTextColor   = XMTheme.inactiveTextColor XMTheme.xmTheme
    }

-------------------------------------------------------------------------------
    -- Layouts

tall :: ModifiedLayout Rename (IfMax ResizableTall ResizableTall) Window
tall = renamed [Replace "[]="]
    $ IfMax 3 (ResizableTall 1 0.03 0.5 []) (ResizableTall 2 0.03 0.5 [])

single :: ModifiedLayout Rename ResizableTall Window
single = renamed [Replace "M|"] $ ResizableTall 1 0.03 0.8 []

monocle
    :: ModifiedLayout
           Rename
           ( ModifiedLayout
                 (Decoration TabbedDecoration DefaultShrinker)
                 Simplest
           )
           Window
monocle = renamed [Replace "[M]"] $ tabbed shrinkText xmDecorationTheme

combo
    :: ModifiedLayout
           Rename
           ( CombineTwo
                 (Tall ())
                 ( ModifiedLayout
                       (Decoration TabbedDecoration DefaultShrinker)
                       Simplest
                 )
                 ( ModifiedLayout
                       (Decoration TabbedDecoration DefaultShrinker)
                       Simplest
                 )
           )
           Window
combo = renamed
    [Replace "[D]"]
    (combineTwo
        (Tall 1 0.03 0.5)
        (tabbed shrinkText xmDecorationTheme)
        (tabbed shrinkText xmDecorationTheme)
    )

horizontal :: ModifiedLayout Rename (Mirror ResizableTall) Window
horizontal =
    renamed [Replace "TTT"] $ Mirror (ResizableTall 1 0.03 0.5 [])

threeCol :: ModifiedLayout Rename ResizableThreeCol Window
threeCol =
    renamed [Replace "|M|"] $ ResizableThreeColMid 1 0.03 0.5 []

floatL
    :: ModifiedLayout
           Rename
           (ModifiedLayout WindowArranger SimplestFloat)
           Window
floatL = renamed [Replace ">>="] simplestFloat

tatami :: ModifiedLayout Rename Tatami Window
tatami = renamed [Replace "M-|"] $ Tatami 1 0.03 0.5

-------------------------------------------------------------------------------
    -- Per Workspace Combinations

alpha = floatL ||| tall ||| monocle
beta = single ||| monocle ||| combo
others =
    tall ||| horizontal ||| combo ||| monocle ||| tatami ||| threeCol ||| floatL

-------------------------------------------------------------------------------
    -- Layout Hook

xmLayouts =
    onWorkspace (head xmWorkspaces) alpha
        $ onWorkspace (xmWorkspaces !! 1) beta others

applySpacing
    :: Integer
    -> l Window
    -> ModifiedLayout Rename (ModifiedLayout Spacing l) Window
applySpacing sz =
    renamed [CutWordsLeft 1]
        . spacingRaw True (Border sz sz sz sz) True (Border sz sz sz sz) True

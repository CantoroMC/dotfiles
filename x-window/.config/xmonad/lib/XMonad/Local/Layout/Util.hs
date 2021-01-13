{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module XMonad.Local.Layout.Util
    ( applySpacing
    , xmLayouts
    ) where

import XMonad hiding ( (|||) )

import XMonad.Layout.Spacing
    ( Spacing (..)
    , Border (..)
    , spacingRaw
    )
import XMonad.Layout.LayoutModifier
    ( ModifiedLayout
    )
import XMonad.Layout.Renamed
    ( Rename
        ( CutWordsLeft
        , Replace
        )
    , renamed
    )
import XMonad.Layout.LayoutCombinators
    ( (|||)
    ) -- TODO: Look at it (and replace maybe combo)
import XMonad.Layout.Combo
    ( CombineTwo
    , combineTwo
    )
import XMonad.Layout.ResizableTile
    ( ResizableTall (..)
    )
import XMonad.Layout.SimplestFloat
    ( SimplestFloat
    , simplestFloat
    )
import XMonad.Layout.WindowArranger
    ( WindowArranger
    )
import XMonad.Layout.TwoPanePersistent
    (TwoPanePersistent (..)
    )
import XMonad.Layout.ThreeColumns
    ( ThreeCol
        (ThreeColMid
        )
    )
import XMonad.Layout.Tabbed
    ( TabbedDecoration (..)
    , tabbed
    , shrinkText
    )
import XMonad.Layout.Decoration
    ( Theme (..)
    , Decoration
    , DefaultShrinker
    )
import XMonad.Layout.Simplest
    ( Simplest (..)
    )

import qualified XMonad.Local.Config.Theme as XMTheme

xmDecorationTheme :: Theme
xmDecorationTheme = def
    { fontName            = "xft:Operator Mono Lig:pixelsize=11"
    , decoHeight          = 15
    , activeColor         = XMTheme.activeColor XMTheme.xmTheme
    , inactiveColor       = XMTheme.inactiveColor XMTheme.xmTheme
    , inactiveBorderColor = XMTheme.inactiveBorderColor XMTheme.xmTheme
    , activeBorderColor   = XMTheme.activeBorderColor XMTheme.xmTheme
    , activeBorderWidth   = XMTheme.borderWidth XMTheme.xmTheme
    , inactiveBorderWidth = XMTheme.borderWidth XMTheme.xmTheme
    , activeTextColor     = XMTheme.activeTextColor XMTheme.xmTheme
    , inactiveTextColor   = XMTheme.inactiveTextColor XMTheme.xmTheme
    }

tall :: ModifiedLayout Rename ResizableTall Window
tall = renamed [Replace "[T]"] $ ResizableTall 1 0.03 0.5 []

monocle :: ModifiedLayout Rename
    (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest) Window
monocle = renamed [Replace "[M]"] $ tabbed shrinkText xmDecorationTheme

combo :: ModifiedLayout Rename
    (CombineTwo (Tall ())
        (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest)
        (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest)) Window
combo = renamed [Replace "[C]"]
    (combineTwo (Tall 1 0.03 0.5)
        (tabbed shrinkText xmDecorationTheme)
        (tabbed shrinkText xmDecorationTheme)
    )

comboGrid :: ModifiedLayout Rename
    (CombineTwo (Tall ())
        (CombineTwo (Mirror Tall ())
            (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest)
            (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest))
        (CombineTwo (Mirror Tall ())
            (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest)
            (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest))) Window
comboGrid = renamed [Replace "[*]"]
    (combineTwo (Tall 1 0.03 0.5)
        (combineTwo (Mirror $ Tall 1 0.03 0.5)
            (tabbed shrinkText xmDecorationTheme)
            (tabbed shrinkText xmDecorationTheme))
        (combineTwo (Mirror $ Tall 1 0.03 0.5)
            (tabbed shrinkText xmDecorationTheme)
            (tabbed shrinkText xmDecorationTheme)))

twoPane :: ModifiedLayout Rename TwoPanePersistent Window
twoPane = renamed [Replace "[||]"] $ TwoPanePersistent Nothing 0.03 0.5

horizontal :: ModifiedLayout Rename (Mirror ResizableTall) Window
horizontal = renamed [Replace "[TT]"] $ Mirror (ResizableTall 1 0.03 0.5 [])

threeCol :: ModifiedLayout Rename ThreeCol Window
threeCol = renamed [Replace "[|||]"] $ ThreeColMid 1 0.03 0.5

floatL :: ModifiedLayout Rename (ModifiedLayout WindowArranger SimplestFloat) Window
floatL = renamed [Replace "[>>=]"] simplestFloat

xmLayouts =
    tall
    ||| combo
    ||| comboGrid
    ||| monocle
    ||| twoPane
    ||| horizontal
    ||| threeCol
    ||| floatL

applySpacing :: Integer -> l Window -> ModifiedLayout Rename (ModifiedLayout Spacing l) Window
applySpacing sz = renamed [CutWordsLeft 1] .
    spacingRaw True (Border sz sz sz sz) True (Border sz sz sz sz) True

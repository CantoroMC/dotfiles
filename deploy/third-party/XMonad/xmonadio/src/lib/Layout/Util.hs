{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-------------------------------------------------------------------------------
    -- Imports
module Layout.Util
    ( applySpacing
    , xmLayouts
    ) where

import           XMonad                  hiding ( (|||) )

import           XMonad.Layout.Combo            ( CombineTwo
                                                , combineTwo
                                                )
import           XMonad.Layout.Decoration       ( Decoration
                                                , DefaultShrinker
                                                , Theme(..)
                                                )

import           XMonad.Layout.IfMax            ( IfMax(..) )

import           XMonad.Layout.LayoutCombinators
                                                ( (|||) )
import           XMonad.Layout.LayoutModifier   ( ModifiedLayout )
import           XMonad.Layout.PerWorkspace     ( onWorkspace )
import           XMonad.Layout.Renamed          ( Rename(CutWordsLeft, Replace)
                                                , renamed
                                                )
import           XMonad.Layout.ResizableThreeColumns
                                                ( ResizableThreeCol
                                                    ( ResizableThreeColMid
                                                    )
                                                )
import           XMonad.Layout.ResizableTile    ( ResizableTall(..) )
import           XMonad.Layout.Simplest         ( Simplest(..) )
import           XMonad.Layout.SimplestFloat    ( SimplestFloat
                                                , simplestFloat
                                                )
import           XMonad.Layout.Spacing          ( Border(..)
                                                , Spacing(..)
                                                , spacingRaw
                                                )
import           XMonad.Layout.Tabbed           ( TabbedDecoration(..)
                                                , shrinkText
                                                , tabbed
                                                )

import           XMonad.Layout.WindowArranger   ( WindowArranger )

import qualified Config.Theme                  as XMTheme
import           Config.Workspace               ( xmWorkspaces )
import           Layout.Tatami                  ( Tatami(..) )

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

tall = renamed [Replace "Tall"]
    $ IfMax 3 (ResizableTall 1 0.03 0.5 []) (ResizableTall 2 0.03 0.4 [])

monocle
    :: ModifiedLayout
           Rename
           ( ModifiedLayout
                 (Decoration TabbedDecoration DefaultShrinker)
                 Simplest
           )
           Window
monocle = renamed [Replace "Monocle"] $ tabbed shrinkText xmDecorationTheme

combo
    :: ModifiedLayout
           Rename
           ( CombineTwo
                 (Tall ())
                 ( ModifiedLayout
                       (Decoration TabbedDecoration DefaultShrinker)
                       Simplest
                 )
                 ( CombineTwo
                       (Mirror Tall ())
                       ( ModifiedLayout
                             (Decoration TabbedDecoration DefaultShrinker)
                             Simplest
                       )
                       ( ModifiedLayout
                             (Decoration TabbedDecoration DefaultShrinker)
                             Simplest
                       )
                 )
           )
           Window
combo = renamed
    [Replace "Combo"]
    (combineTwo
        (Tall 1 0.03 0.5)
        (tabbed shrinkText xmDecorationTheme)
        (combineTwo (Mirror $ Tall 1 0.03 0.5)
                    (tabbed shrinkText xmDecorationTheme)
                    (tabbed shrinkText xmDecorationTheme)
        )
    )

horizontal :: ModifiedLayout Rename (Mirror ResizableTall) Window
horizontal =
    renamed [Replace "MirrorTall"] $ Mirror (ResizableTall 1 0.03 0.5 [])

threeCol :: ModifiedLayout Rename ResizableThreeCol Window
threeCol =
    renamed [Replace "ThreeColumns"] $ ResizableThreeColMid 1 0.03 0.5 []

floatL
    :: ModifiedLayout
           Rename
           (ModifiedLayout WindowArranger SimplestFloat)
           Window
floatL = renamed [Replace "Float >>="] simplestFloat

tatami :: Tatami Window
tatami = Tatami 1 0.03 0.5

-------------------------------------------------------------------------------
    -- IfMax

metamorphic =
    renamed [Replace "Metamorphic"] $ IfMax 3 tall (IfMax 5 tatami threeCol)

matacombico = renamed [Replace "Fisher"] $ IfMax 4 tall combo

-------------------------------------------------------------------------------
    -- Per Workspace Combinations

alpha = floatL ||| tall
beta = matacombico ||| tall ||| monocle ||| floatL
gamma = metamorphic ||| tall ||| combo ||| monocle ||| floatL
others =
    tall ||| tatami ||| combo ||| monocle ||| horizontal ||| threeCol ||| floatL

-------------------------------------------------------------------------------
    -- Layout Hook

xmLayouts =
    onWorkspace (head xmWorkspaces) alpha
        $ onWorkspace (xmWorkspaces !! 1) beta
        $ onWorkspace (xmWorkspaces !! 2) gamma others

applySpacing
    :: Integer
    -> l Window
    -> ModifiedLayout Rename (ModifiedLayout Spacing l) Window
applySpacing sz =
    renamed [CutWordsLeft 1]
        . spacingRaw True (Border sz sz sz sz) True (Border sz sz sz sz) True

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-------------------------------------------------------------------------------
    -- Imports
module XMonad.Local.Layout.Util
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
import           XMonad.Layout.LayoutCombinators
                                                ( (|||) )
import           XMonad.Layout.LayoutModifier   ( ModifiedLayout )
import           XMonad.Layout.PerWorkspace     ( onWorkspace )
import           XMonad.Layout.Renamed          ( Rename(CutWordsLeft, Replace)
                                                , renamed
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
import           XMonad.Layout.ThreeColumns     ( ThreeCol(ThreeColMid) )
import           XMonad.Layout.WindowArranger   ( WindowArranger )

import qualified XMonad.Local.Config.Theme     as XMTheme
import           XMonad.Local.Config.Workspace  ( xmWorkspaces )
import           XMonad.Local.Layout.Tatami     ( Tatami(..) )

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
    , activeBorderWidth   = XMTheme.borderWidth XMTheme.xmTheme
    , inactiveBorderWidth = XMTheme.borderWidth XMTheme.xmTheme
    , activeTextColor     = XMTheme.activeTextColor XMTheme.xmTheme
    , inactiveTextColor   = XMTheme.inactiveTextColor XMTheme.xmTheme
    }

-------------------------------------------------------------------------------
    -- Layouts

tall :: ModifiedLayout Rename ResizableTall Window
tall = renamed [Replace "Tall"] $ ResizableTall 1 0.03 0.5 []

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

threeCol :: ModifiedLayout Rename ThreeCol Window
threeCol = renamed [Replace "ThreeColumns"] $ ThreeColMid 1 0.03 0.5

floatL
    :: ModifiedLayout
           Rename
           (ModifiedLayout WindowArranger SimplestFloat)
           Window
floatL = renamed [Replace "Float >>="] simplestFloat

tatami :: Tatami Window
tatami = Tatami 1 0.03 0.5

-------------------------------------------------------------------------------
    -- Per Workspace Combinations

alpha    = monocle    ||| floatL
beta     = tall       ||| monocle ||| combo
gamma    = tatami     ||| tall    ||| combo
eta      = combo      ||| tall
theta    = horizontal ||| tall
iota     = floatL
others   = tall       ||| tatami  ||| combo ||| monocle ||| horizontal ||| threeCol ||| floatL

-------------------------------------------------------------------------------
    -- Layout Hook

xmLayouts =
    onWorkspace (head xmWorkspaces) alpha
        $ onWorkspace (xmWorkspaces !! 1) beta
        $ onWorkspace (xmWorkspaces !! 2) gamma
        $ onWorkspace (xmWorkspaces !! 6) eta
        $ onWorkspace (xmWorkspaces !! 7) theta
        $ onWorkspace (xmWorkspaces !! 8) iota others

applySpacing
    :: Integer
    -> l Window
    -> ModifiedLayout Rename (ModifiedLayout Spacing l) Window
applySpacing sz =
    renamed [CutWordsLeft 1]
        . spacingRaw True (Border sz sz sz sz) True (Border sz sz sz sz) True

module Layout.Layouts
    ( xwmTall
    , xwmMirrorTall
    , xwmThreeCol
    , xwmTatami
    , xwmTabbed
    , xwmFloat
    , xwmCombo
    , applySpacing
    ) where



import XMonad

import XMonad.Layout.NoBorders (SmartBorder, smartBorders, WithBorder, noBorders)
import XMonad.Layout.Combo (CombineTwo, combineTwo)
import XMonad.Layout.Decoration (Decoration, DefaultShrinker, Theme(..))
import XMonad.Layout.IfMax (IfMax(..))
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Renamed (Rename(Replace,CutWordsLeft), renamed)
import XMonad.Layout.Spacing (Border(..), Spacing(..), spacingRaw)

import XMonad.Layout.ResizableThreeColumns (ResizableThreeCol(ResizableThreeColMid))
import XMonad.Layout.ResizableTile (ResizableTall(ResizableTall))
import XMonad.Layout.Tabbed (TabbedDecoration(..), shrinkText, tabbed)
import XMonad.Layout.Simplest (Simplest(..))
import XMonad.Layout.SimplestFloat (SimplestFloat, simplestFloat)
import XMonad.Layout.WindowArranger (WindowArranger)

import qualified Config.Theme as XwmTheme
import Layout.Tatami (Tatami(..))



xwmTall :: ModifiedLayout SmartBorder
    (ModifiedLayout Rename (IfMax ResizableTall ResizableTall)) Window
xwmTall = smartBorders . renamed [Replace "ResizableTall"] $ IfMax 3
    (ResizableTall nmaster delta ratio [])
    (ResizableTall (nmaster+1) delta ratio [])
      where
        nmaster = 1
        delta   = 3 / 100
        ratio   = 1 / 2

xwmMirrorTall :: ModifiedLayout SmartBorder (Mirror ResizableTall) Window
xwmMirrorTall = smartBorders $ Mirror (ResizableTall nmaster delta ratio [])
  where
    nmaster = 1
    delta   = 3 / 100
    ratio   = 1 / 2

xwmThreeCol :: ModifiedLayout SmartBorder ResizableThreeCol Window
xwmThreeCol = smartBorders $ ResizableThreeColMid nmaster delta ratio []
  where
    nmaster = 1
    delta   = 3 / 100
    ratio   = 1 / 2

xwmTatami :: ModifiedLayout SmartBorder Tatami Window
xwmTatami = smartBorders $ Tatami nmaster delta ratio
  where
    nmaster = 1
    delta   = 3 / 100
    ratio   = 1 / 2

xwmDecorationTheme :: Theme
xwmDecorationTheme = def
    { fontName            = "xft:Operator Mono Lig:pixelsize=9"
    , decoHeight          = 15
    , decoWidth           = 200
    , activeColor         = XwmTheme.activeColor XwmTheme.xwmTheme
    , inactiveColor       = XwmTheme.inactiveColor XwmTheme.xwmTheme
    , urgentColor         = XwmTheme.urgentColor XwmTheme.xwmTheme
    , inactiveBorderColor = XwmTheme.inactiveBorderColor XwmTheme.xwmTheme
    , urgentBorderColor   = XwmTheme.urgentBorderColor XwmTheme.xwmTheme
    , activeBorderColor   = XwmTheme.activeBorderColor XwmTheme.xwmTheme
    , activeBorderWidth   = XwmTheme.borderWidth XwmTheme.xwmTheme
    , inactiveBorderWidth = XwmTheme.borderWidth XwmTheme.xwmTheme
    , urgentBorderWidth   = XwmTheme.borderWidth XwmTheme.xwmTheme + 1
    , activeTextColor     = XwmTheme.activeTextColor XwmTheme.xwmTheme
    , inactiveTextColor   = XwmTheme.inactiveTextColor XwmTheme.xwmTheme
    , urgentTextColor     = XwmTheme.urgentTextColor XwmTheme.xwmTheme
    }

xwmTabbed :: ModifiedLayout WithBorder (ModifiedLayout Rename
    ( ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest))
    Window
xwmTabbed = noBorders . renamed [Replace "Tabbed"] $
    tabbed shrinkText xwmDecorationTheme

xwmFloat :: ModifiedLayout WithBorder
    (ModifiedLayout Rename (ModifiedLayout WindowArranger SimplestFloat)) Window
xwmFloat = noBorders . renamed [Replace "Float"] $ simplestFloat


xwmCombo :: ModifiedLayout SmartBorder (ModifiedLayout Rename
    (CombineTwo (Tall ())
        (Mirror ResizableTall)
        (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest))
    )
    Window
xwmCombo = smartBorders . renamed [Replace "Combo"] $
    combineTwo (Tall 1 0.03 0.5)
        (Mirror $ ResizableTall 1 0.03 0.5 [])
        (tabbed shrinkText xwmDecorationTheme)


applySpacing :: Integer -> l Window -> ModifiedLayout Rename (ModifiedLayout Spacing l) Window
applySpacing sz = renamed [CutWordsLeft 1] .
    spacingRaw False (Border sz sz sz sz) True (Border sz sz sz sz) True

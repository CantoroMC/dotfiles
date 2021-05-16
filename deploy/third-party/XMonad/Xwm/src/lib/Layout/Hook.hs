{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Layout.Hook
    ( xwmLayoutHook
    ) where


import XMonad

import XMonad.Layout.ResizableThreeColumns (ResizableThreeCol(ResizableThreeColMid))
import XMonad.Layout.ResizableTile (ResizableTall(ResizableTall))

import XMonad.Util.Types (Direction2D (..))
import XMonad.Hooks.ManageDocks (avoidStrutsOn)

xwmLayoutHook = avoidStrutsOn [U] $ tiled ||| horizontal ||| threecol ||| Full
  where
    tiled         = ResizableTall nmaster delta ratio []
    horizontal    = Mirror tiled
    threecol      = ResizableThreeColMid nmaster delta ratio []
    nmaster       = 1
    ratio         = 1 / 2
    delta         = 3 / 100

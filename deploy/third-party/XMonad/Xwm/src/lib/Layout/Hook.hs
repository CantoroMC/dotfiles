{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Layout.Hook
    ( xmLayoutHook
    ) where


import XMonad

import XMonad.Layout.ResizableThreeColumns
    (ResizableThreeCol(ResizableThreeColMid))
import XMonad.Layout.ResizableTile (ResizableTall(ResizableTall))

import XMonad.Layout.Magnifier (magnifiercz')



xmLayoutHook = tiled ||| horizontal ||| threecol ||| Full
  where
    tiled         = magnification $ ResizableTall nmaster delta ratio []
    horizontal    = magnification $ Mirror tiled
    threecol      = magnification $ ResizableThreeColMid nmaster delta ratio []
    nmaster       = 1
    ratio         = 1 / 2
    delta         = 3 / 100
    magnification = magnifiercz' 1.3

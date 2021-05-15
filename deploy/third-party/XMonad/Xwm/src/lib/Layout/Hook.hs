{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Layout.Hook
    ( xmLayoutHook
    ) where


import XMonad

import XMonad.Layout.ResizableThreeColumns (ResizableThreeCol(ResizableThreeColMid))
import XMonad.Layout.ResizableTile (ResizableTall(ResizableTall))

-- import XMonad.Util.Types (Direction2D (..))
-- import XMonad.Hooks.ManageDocks (avoidStrutsOn)

-- xmLayoutHook = avoidStrutsOn [U] $ tiled ||| horizontal ||| threecol ||| Full
xmLayoutHook = tiled ||| horizontal ||| threecol ||| Full
  where
    tiled         = ResizableTall nmaster delta ratio []
    horizontal    = Mirror tiled
    threecol      = ResizableThreeColMid nmaster delta ratio []
    nmaster       = 1
    ratio         = 1 / 2
    delta         = 3 / 100

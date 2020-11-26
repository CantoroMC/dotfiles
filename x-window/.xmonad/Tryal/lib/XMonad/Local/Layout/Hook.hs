{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module XMonad.Local.Layout.Hook
    ( xmLayoutHook
    ) where

import XMonad.Hooks.ManageDocks
    ( avoidStrutsOn
    )
import XMonad.Layout.WindowNavigation
    ( windowNavigation
    )
import XMonad.Layout.NoBorders
    ( smartBorders
    )
import XMonad.Util.Types
    ( Direction2D (..)
    )

import XMonad.Local.Layout.Util
    ( applySpacing
    , xmLayouts
    )

xmLayoutHook =
    avoidStrutsOn [U] . smartBorders . windowNavigation . applySpacing 0 $ xmLayouts

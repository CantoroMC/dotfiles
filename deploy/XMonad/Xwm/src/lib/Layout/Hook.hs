{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Layout.Hook
    ( xwmLayoutHook
    ) where



import XMonad.Layout.LayoutCombinators ((|||))
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.WindowNavigation (windowNavigation)

import XMonad.Hooks.ManageDocks (avoidStrutsOn, AvoidStruts)
import XMonad.Util.Types (Direction2D(U))

import Config.Workspaces (xwmWorkspaces)
import Layout.Layouts
    ( xwmTall
    , xwmMirrorTall
    , xwmThreeCol
    , xwmTatami
    , xwmTabbed
    , xwmFloat
    , xwmCombo
    , applySpacing
    )



-------------------------------------------------------------------------------
    -- Per Workspace Combinations
xwmLayout =
    xwmTall
    ||| xwmCombo
    ||| xwmThreeCol
    ||| xwmMirrorTall
    ||| xwmTabbed
    ||| xwmTatami
    ||| xwmFloat
xwmFloatWSL = xwmFloat ||| xwmTall ||| xwmTabbed
xwmComboWSL =
    xwmCombo
    ||| xwmTall
    ||| xwmThreeCol
    ||| xwmMirrorTall
    ||| xwmTabbed
    ||| xwmTatami
    ||| xwmFloat
xwmFullWSL =
    xwmTabbed
    ||| xwmTall
    ||| xwmCombo
    ||| xwmThreeCol
    ||| xwmMirrorTall
    ||| xwmTatami
    ||| xwmFloat

xwmLayouts =
    onWorkspace (last xwmWorkspaces) xwmFloatWSL
        $ onWorkspace (xwmWorkspaces !! 1) xwmFullWSL
        $ onWorkspace (xwmWorkspaces !! 2) xwmComboWSL
        xwmLayout

-------------------------------------------------------------------------------
    -- Layout Hook
xwmLayoutHook = avoidStrutsOn [U] .  windowNavigation . applySpacing 0 $ xwmLayouts

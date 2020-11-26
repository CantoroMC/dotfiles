{-# LANGUAGE FlexibleContexts #-}

module XMonad.Local.Urgency.Hook
    ( applyUrgencyHook
    ) where

import XMonad
import XMonad.Hooks.UrgencyHook
    ( BorderUrgencyHook (..)
    , UrgencyConfig (..)
    , SuppressWhen (..)
    , RemindWhen (..)
    , withUrgencyHookC
    )

import qualified XMonad.Local.Config.Theme as XMTheme

xmUrgencyHook :: BorderUrgencyHook
xmUrgencyHook = BorderUrgencyHook
    { urgencyBorderColor = XMTheme.urgentBorderColor XMTheme.xmTheme
    }

xmUrgencyConfig :: UrgencyConfig
xmUrgencyConfig = UrgencyConfig
    { suppressWhen = Focused
    , remindWhen = Repeatedly 3 30}

applyUrgencyHook :: LayoutClass l Window => XConfig l -> XConfig l
applyUrgencyHook = withUrgencyHookC xmUrgencyHook xmUrgencyConfig

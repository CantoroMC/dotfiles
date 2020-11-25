module XMonad.Local.Log.Hook
    ( xmLogHook
    ) where

import GHC.IO.Handle
    ( Handle
    )

import XMonad
import XMonad.Hooks.DynamicLog
    ( dynamicLogWithPP
    )

import XMonad.Local.Log.XMobar
    ( xmXMobarPP
    )

xmLogHook :: Handle -> X ()
xmLogHook = dynamicLogWithPP . xmXMobarPP

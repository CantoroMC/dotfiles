module Log.Hook
    ( xmLogHook
    ) where

import GHC.IO.Handle
    ( Handle
    )

import XMonad
import XMonad.Hooks.DynamicLog
    ( dynamicLogWithPP
    )

import Log.XMobar
    ( xmXMobarPP
    )

xmLogHook :: Handle -> X ()
xmLogHook = dynamicLogWithPP . xmXMobarPP

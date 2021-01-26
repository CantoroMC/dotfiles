module Bindings.Mouse
    ( xmMouseBindings
    ) where

import qualified Data.Map as Map
    ( Map
    , fromList
    )

import XMonad
import qualified XMonad.StackSet as XMSS

xmMouseBindings :: XConfig l -> Map.Map (KeyMask, Button) (Window -> X ())
xmMouseBindings XConfig {XMonad.modMask = modm} = Map.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows XMSS.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows XMSS.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                      >> windows XMSS.shiftMaster)
    ]

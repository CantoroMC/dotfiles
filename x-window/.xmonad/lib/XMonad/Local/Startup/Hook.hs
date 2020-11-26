module XMonad.Local.Startup.Hook
    ( xmStartupHook
    ) where

import XMonad
import qualified XMonad.StackSet as XMSS
import XMonad.Util.Cursor
    ( setDefaultCursor
    )
import XMonad.Util.SpawnOnce
    ( spawnOnce
    )

import XMonad.Local.Config.Workspace
    ( Workspace (..)
    )

xmStartupHook :: X ()
xmStartupHook = do
    fixSupportedAtoms
    setDefaultCursor xC_left_ptr
    spawnOnce "xmobar \"${XDG_CONFIG_HOME}\"/xmobar/xmobar_bot.hs"
    windows . XMSS.greedyView $ show WsWriting

-- | Detect urgency of some programs like kitty (not covered in 'XMonad.Hooks.EwmhDesktops.ewmh'):
-- https://github.com/kovidgoyal/kitty/issues/1016#issuecomment-480472827
fixSupportedAtoms :: X ()
fixSupportedAtoms = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- mapM getAtom
        [ "_NET_WM_STATE"
        , "_NET_WM_STATE_DEMANDS_ATTENTION"
        ]
    io $ changeProperty32 dpy r a c propModeAppend (fmap fromIntegral supp)

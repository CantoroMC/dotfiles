{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module XMonad.Local.Bindings.Util
    ( Direction (..)
    , moveFloating
    , resizeFloating
    , terminalFromConf
    , inTerminalFromConf
    ) where

import XMonad

import XMonad.Actions.FloatKeys
    ( keysMoveWindow
    , keysResizeWindow
    )

------------------------------------------------------------------------------
    -- User
data Direction = FL
               | FD
               | FU
               | FR

moveFloating :: Direction -> Window -> X ()
moveFloating d = keysMoveWindow (direction d)

resizeFloating :: Direction -> Window -> X ()
resizeFloating d = keysResizeWindow (direction d) (0 , 0)

direction :: Direction -> D
direction d = (dx , dy)
  where (dx , dy) = case d of FL -> (-pixel , 0)
                              FD -> (0 , pixel)
                              FU -> (0 , -pixel)
                              FR -> (pixel , 0)
        pixel = 20

terminalFromConf :: (MonadIO m, MonadReader XConf m) => m String
terminalFromConf = reader $ terminal . config

inTerminalFromConf :: (MonadIO m, MonadReader XConf m) => String -> m String
inTerminalFromConf prog = do terminalEmulator <- terminalFromConf
                             return $ terminalEmulator <> " -t " <> prog <> " " <> prog

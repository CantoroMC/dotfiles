module XMonad.Local.Config.Workspace
    ( Workspace (..)
    , workspaceIds
    ) where

import XMonad

data Workspace = WsHome
               | WsReading
               | WsWriting
               | WsBrowser
               | WsMultimedia
               | WsSix
               | WsSeven
               | WsEight
               | WsSupport
               deriving (Eq, Ord, Read, Show, Enum, Bounded)

workspaceIds :: [WorkspaceId]
workspaceIds = show <$> wss
    where wss = [ minBound .. maxBound ] :: [Workspace]

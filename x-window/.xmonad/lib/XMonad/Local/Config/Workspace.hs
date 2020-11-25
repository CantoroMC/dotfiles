module XMonad.Local.Config.Workspace
    ( Workspace (..)
    , xmWorkspaces
    ) where

import XMonad

data Workspace =
    WsHome
    | WsReading
    | WsWriting
    | WsBrowser
    | WsMultimedia
    | WsSix
    | WsSeven
    | WsEight
    | WsSupport
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

xmWorkspaces :: [WorkspaceId]
xmWorkspaces = show <$> wss
    where wss = [ minBound .. maxBound ] :: [Workspace]

module Config.Workspace
    ( Workspace(..)
    , xmWorkspaces
    ) where

import XMonad

data Workspace =
    Alpha
    | Beta
    | Gamma
    | Delta
    | Epsilon
    | Zeta
    | Eta
    | Theta
    | Iota
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

xmWorkspaces :: [WorkspaceId]
xmWorkspaces = show <$> wss where wss = [minBound .. maxBound] :: [Workspace]

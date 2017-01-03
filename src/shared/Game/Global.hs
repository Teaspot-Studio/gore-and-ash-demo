module Game.Global(
    GameGlobal(..)
  , globalId
  ) where

import Data.Map.Strict (Map)
import Data.Store
import GHC.Generics

import Game.GoreAndAsh.Sync

import Game.Player

-- | Global game state
data GameGlobal = GameGlobal {
  gameScore :: Map PlayerId Int
} deriving (Generic, Show)

instance Store GameGlobal

-- | ID of shared game global info
globalId :: SyncItemId
globalId = 0
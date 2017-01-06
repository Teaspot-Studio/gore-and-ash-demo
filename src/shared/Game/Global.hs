module Game.Global(
    GameGlobal(..)
  , globalId
  ) where

import Data.Store
import GHC.Generics

import Game.GoreAndAsh.Sync


-- | Global game state
data GameGlobal = GameGlobal {

} deriving (Generic, Show)

instance Store GameGlobal

-- | ID of shared game global info
globalId :: SyncItemId
globalId = 0
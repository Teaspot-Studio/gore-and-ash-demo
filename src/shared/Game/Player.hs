module Game.Player(
    PlayerId(..)
  , Player(..)
  , playerCollectionId
  ) where

import GHC.Generics
import Data.Store
import Linear

import Game.GoreAndAsh.Sync

-- | Unique player id
newtype PlayerId = PlayerId { unPlayerId :: Int }
  deriving (Generic, Show, Eq, Ord)

instance Store PlayerId

-- | Shared player info
data Player s = Player {
-- | Player position
  playerPos    :: !(V2 Double)
-- | Player color
, playerColor  :: !(V3 Double)
-- | Player absolute speed
, playerSpeed  :: !Double
-- | Player size
, playerSize   :: !Double
-- | Player other information
, playerCustom :: !s
} deriving (Generic, Show)

-- | ID of shared player collection
playerCollectionId :: SyncItemId
playerCollectionId = 1
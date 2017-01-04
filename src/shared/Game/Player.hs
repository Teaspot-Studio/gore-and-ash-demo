module Game.Player(
    PlayerId(..)
  , Player(..)
  , playerCollectionId
  , playerCommandId
  , playerPosId
  , playerColorId
  , playerSpeedId
  , playerSizeId
  , PlayerCommand(..)
  ) where

import Data.Store
import GHC.Generics
import Linear
import Store()

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
} deriving (Generic)

instance Functor Player where
  fmap f p = p { playerCustom = f $ playerCustom p }

-- | ID of shared player collection
playerCollectionId :: SyncItemId
playerCollectionId = 1

playerPosId, playerColorId, playerSpeedId, playerSizeId, playerCommandId :: SyncItemId
playerPosId     = 1
playerColorId   = 2
playerSpeedId   = 3
playerSizeId    = 4
playerCommandId = 5

-- | Commands to client side
data PlayerCommand =
    RequestPlayerId       -- ^ Ask server for player id
  | YourPlayerId PlayerId -- ^ Inform about client player id
  deriving (Generic)

instance Store PlayerCommand
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

import Game.GoreAndAsh
import Game.GoreAndAsh.Sync

-- | Unique player id
newtype PlayerId = PlayerId { unPlayerId :: Int }
  deriving (Generic, Show, Eq, Ord)

instance Store PlayerId

-- | Shared player info
data Player t s = Player {
-- | Player position
  playerPos    :: !(Dynamic t (V2 Double))
-- | Player color
, playerColor  :: !(Dynamic t (V3 Double))
-- | Player absolute speed
, playerSpeed  :: !(Dynamic t Double)
-- | Player size
, playerSize   :: !(Dynamic t Double)
-- | Player other information
, playerCustom :: !s
} deriving (Generic)

instance Functor (Player t) where
  fmap f p = p { playerCustom = f $ playerCustom p }

-- | ID of shared player collection
playerCollectionId :: SyncItemId
playerCollectionId = 1

playerPosId, playerColorId, playerSpeedId, playerSizeId, playerCommandId :: SyncItemId
playerCommandId = 0
playerPosId     = 1
playerColorId   = 2
playerSpeedId   = 3
playerSizeId    = 4

-- | Commands to client side
data PlayerCommand = YourPlayerId PlayerId -- ^ Inform about client player id
  deriving (Generic)

instance Store PlayerCommand
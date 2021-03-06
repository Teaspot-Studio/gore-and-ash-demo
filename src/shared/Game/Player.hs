module Game.Player(
    PlayerId(..)
  , Player(..)
  , playerCollectionId
  , playerCommandId
  , playerPosId
  , playerColorId
  , playerSpeedId
  , playerSizeId
  , playerScoreId
  , PlayerCommand(..)
  , printPlayer
  , playerShootRatio
  ) where

import Data.Monoid
import Data.Store
import GHC.Generics
import Linear
import Store()

import Game.GoreAndAsh
import Game.GoreAndAsh.Sync
import Game.GoreAndAsh.Logging

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
-- | Player game score
, playerScore  :: !Int
-- | Player other information
, playerCustom :: !s
} deriving (Generic)

instance Functor Player where
  fmap f p = p { playerCustom = f $ playerCustom p }

-- | ID of shared player collection
playerCollectionId :: SyncItemId
playerCollectionId = 1

playerPosId, playerColorId, playerSpeedId, playerSizeId, playerCommandId, playerScoreId :: SyncItemId
playerPosId     = 1
playerColorId   = 2
playerSpeedId   = 3
playerSizeId    = 4
playerCommandId = 5
playerScoreId   = 6

-- | Commands to client side
data PlayerCommand =
    RequestPlayerId         -- ^ Ask server for player id
  | YourPlayerId PlayerId   -- ^ Inform about client player id
  | PlayerShoot (V2 Double) -- ^ Request spawning of bullet in given direction
  deriving (Generic)

instance Store PlayerCommand

-- | Display info about player
printPlayer :: (LoggingMonad t m, Show s) => PlayerId -> Dynamic t (Player s) -> m ()
printPlayer i pdyn =
  logInfoE $ ffor (updated pdyn) $ \Player{..} -> "Player:\n"
    <> "\tid:    " <> showl i   <> "\n"
    <> "\tpos:   " <> showl playerPos <> "\n"
    <> "\tcolor: " <> showl playerColor <> "\n"
    <> "\tspd:   " <> showl playerSpeed <> "\n"
    <> "\tsize:  " <> showl playerSize <> "\n"
    <> "\tcustom:" <> showl playerCustom

-- | Number of bullets in second that player can shoot
playerShootRatio :: Int
playerShootRatio = 3

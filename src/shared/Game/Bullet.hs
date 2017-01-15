module Game.Bullet(
    Bullet(..)
  , BulletId(..)
  , CreateBullet(..)
  , bulletCollectionId
  , bulletVelId
  , bulletPosId
  , bulletPlayerId
  , bulletLifeTimeId
  ) where

import Data.Store
import Game.Player
import GHC.Generics
import Linear

import Game.GoreAndAsh.Sync

-- | Bullet information
data Bullet s = Bullet {
  bulletVel      :: !(V2 Double) -- ^ Velocity
, bulletPos      :: !(V2 Double) -- ^ Position
, bulletPlayer   :: !PlayerId    -- ^ Owner player
, bulletLifeTime :: !Double      -- ^ Time left to live
, bulletCustom   :: !s           -- ^ Custom data for bullet
} deriving (Generic, Show, Eq)

instance Functor Bullet where
  fmap f b = b { bulletCustom = f $ bulletCustom b }

-- | Unique bullet ID
newtype BulletId = BulletId { unBulletId :: Int }
  deriving (Generic, Show, Eq, Ord)

instance Store BulletId

-- | Info required to create new bullet
data CreateBullet = CreateBullet {
  createBulletPos    :: !(V2 Double) -- ^ Bullet spawn position
, createBulletDir    :: !(V2 Double) -- ^ Bullet flight direction
, createBulletVel    :: !Double      -- ^ Initial bullet speed absolute value
, createBulletPlayer :: !PlayerId    -- ^ Bullet owner
} deriving (Generic, Show)

instance Store CreateBullet

-- | Unique collection id for bullets
bulletCollectionId :: SyncItemId
bulletCollectionId = 2

-- | IDs for fields of 'Bullet'
bulletVelId, bulletPosId, bulletPlayerId, bulletLifeTimeId :: SyncItemId
bulletVelId = 0
bulletPosId = 1
bulletPlayerId = 2
bulletLifeTimeId = 3

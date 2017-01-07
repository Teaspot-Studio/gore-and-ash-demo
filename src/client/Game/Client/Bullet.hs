module Game.Client.Bullet(
    handleBullets
  , ClientBullet
  ) where

import Control.Monad
import Data.Map.Strict (Map)
import Data.Monoid
import Linear

import Game.Bullet
import Game.GoreAndAsh
import Game.GoreAndAsh.Sync
import Game.Monad

-- | Client side extension of bullet
type ClientBullet = Bullet ()

-- | Process all bullets, sync them from server
handleBullets :: AppFrame t => AppMonad t (Dynamic t (Map BulletId ClientBullet))
handleBullets = do
  joinDynThroughMap <$> remoteCollection bulletCollectionId bullet

-- | Controller of single bullet
bullet :: forall t . AppFrame t
  => BulletId     -- ^ ID of created bullet
  -> CreateBullet -- ^ Creation info for bullet
  -> AppMonad t (Dynamic t ClientBullet)
bullet i CreateBullet{..} = do
  syncBullet
  where
    bulletSpd = 10
    initBullet = Bullet {
        bulletVel = bulletSpd * normalize createBulletDir
      , bulletPos = createBulletPos
      , bulletPlayer = createBulletPlayer
      , bulletLifeTime = 0
      , bulletCustom = ()
      }
    syncBullet = fmap join $ syncWithName (show i) (pure initBullet) $ do
      vel <- syncFromServer bulletVelId (bulletVel initBullet)
      pos <- syncFromServer bulletPosId (bulletPos initBullet)
      return $ Bullet
        <$> vel
        <*> pos
        <*> pure (bulletPlayer initBullet)
        <*> pure (bulletLifeTime initBullet)
        <*> pure (bulletCustom initBullet)

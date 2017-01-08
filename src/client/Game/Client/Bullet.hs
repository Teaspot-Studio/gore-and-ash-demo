module Game.Client.Bullet(
    handleBullets
  , ClientBullet
  ) where

import Control.Monad
import Data.Map.Strict (Map)
import Linear

import Game.Bullet
import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Sync
import Game.GoreAndAsh.Time
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
bullet i CreateBullet{..} = syncBullet
  where
    initBullet = Bullet {
        bulletVel = V2 createBulletVel createBulletVel * normalize createBulletDir
      , bulletPos = createBulletPos
      , bulletPlayer = createBulletPlayer
      , bulletLifeTime = 0
      , bulletCustom = ()
      }
    syncBullet = do
      initDyn <- initialPredict
      fmap join $ syncWithNameWith (show i) (pure initDyn) $ do
        buildE <- getPostBuild
        vel <- syncFromServer bulletVelId (bulletVel initBullet)
        pos <- predictPos vel =<< syncFromServerWith bulletPosId (bulletPos <$> initDyn)
        return $ Bullet
          <$> vel
          <*> pos
          <*> pure (bulletPlayer initBullet)
          <*> pure (bulletLifeTime initBullet)
          <*> pure (bulletCustom initBullet)

    initialPredict = do
      pos <- predictPos (pure $ bulletVel initBullet) (pure $ bulletPos initBullet)
      return $ Bullet
        <$> pure (bulletVel initBullet)
        <*> pos
        <*> pure (bulletPlayer initBullet)
        <*> pure (bulletLifeTime initBullet)
        <*> pure (bulletCustom initBullet)

    predictPos velDyn serverPos = do
      buildE <- getPostBuild
      let bulletSymDt = 0.01 :: Double
          dtV = V2 bulletSymDt bulletSymDt
      tickE <- tickEvery $ realToFrac bulletSymDt
      let predictE = leftmost [tickE, buildE]
      predictM serverPos predictE $ const $ \p -> do
        vel <- sample . current $ velDyn
        return $ p + dtV * vel
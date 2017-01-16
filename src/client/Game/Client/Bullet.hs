module Game.Client.Bullet(
    handleBullets
  , ClientBullet
  ) where

import Control.Monad
import Data.Map.Strict (Map)
import Data.Maybe
import Linear

import qualified Data.Map.Strict as M

import Game.Bullet
import Game.GoreAndAsh
import Game.GoreAndAsh.Sync
import Game.GoreAndAsh.Time
import Game.Monad

-- | Client side extension of bullet
type ClientBullet = Bullet ()

-- | Process all bullets, sync them from server
handleBullets :: AppFrame t => AppMonad t (Dynamic t (Map BulletId ClientBullet))
handleBullets = do
  (bulletsDyn, updMapE) <-  remoteCollection bulletCollectionId bullet
  let delsE = ffor updMapE $ M.filter isNothing
  _ <- syncUnregisterNames $ fmap show . M.keys <$> delsE -- delete sync objects for deleted bullets
  return $ joinDynThroughMap bulletsDyn

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

    -- Complex prediction of bullet position, that assumes stable movement + interpolation
    -- of steep server rejects.
    predictPos :: Dynamic t (V2 Double) -> Dynamic t (V2 Double) -> AppMonad t (Dynamic t (V2 Double))
    predictPos velDyn serverPos = do
      buildE <- getPostBuild
      let bulletSymDt = 0.01 :: Double
          bulletInterpN = 5 -- steps of interpolation
          bulletInterpDt dp = do -- time of interpolation depends on distance
            vel <- sample . current $ velDyn
            -- make bullet fly by 1.5 of velocity on interpolation interval
            let s = norm dp
            return $ if s > 20 || s < 1 then Nothing
              else Just $ realToFrac $ s / (1.5 * norm vel)
          dtV = V2 bulletSymDt bulletSymDt
      tickE <- tickEvery $ realToFrac bulletSymDt
      let predictE = leftmost [tickE, buildE]
      predictInterpolateM bulletInterpN bulletInterpDt serverPos predictE $ const $ \p -> do
        vel <- sample . current $ velDyn
        return $ p + dtV * vel
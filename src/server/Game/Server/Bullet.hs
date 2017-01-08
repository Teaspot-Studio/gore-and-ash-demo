module Game.Server.Bullet(
    processBullets
  , ServerBullet
  ) where

import Control.Monad
import Data.Map.Strict (Map)
import Data.Monoid
import Linear

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M

import Game.GoreAndAsh
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Sync
import Game.GoreAndAsh.Time

import Game.Bullet
import Game.Monad

-- | Specific extended of bullet for server
type ServerBullet = Bullet ()

-- | Process all bullets
processBullets :: forall t f . (AppFrame t, Foldable f)
  => Event t (f CreateBullet) -- ^ Fires when a new bullet need to be spawned
  -> AppMonad t (Dynamic t (Map BulletId ServerBullet))
processBullets createE = do
  -- we need a bullet counter to generate ids
  bulletCounterRef <- newExternalRef (0 :: Int)
  bulletCounter <- externalRefDynamic bulletCounterRef
  -- define creation event that increses counter for ids
  let createBullet ce = do
        i <- sample . current $ bulletCounter
        modifyExternalRef bulletCounterRef $ \v -> (v+1, ())
        return (BulletId i, Just ce)
  addE <- performEvent $ ffor createE $ fmap M.fromList . mapM createBullet . F.toList
  -- recursive dependency as we need to delete bullets that life time is over
  rec
    let delE = fforMaybe (updated bulletMapDyn) $ \bulletMap -> let
            dieMap = fmap (const Nothing) $ M.filter (\Bullet{..} -> bulletLifeTime <= 0) bulletMap
          in if M.null dieMap then Nothing
            else Just dieMap
    -- automatic collection synchronisation
    let updE = addE <> delE
    bulletMapDyn <- joinDynThroughMap <$>
      hostSimpleCollection bulletCollectionId mempty updE bullet
  return bulletMapDyn

-- | Controller of single bullet
bullet :: forall t . AppFrame t
  => BulletId     -- ^ ID of created bullet
  -> CreateBullet -- ^ Creation info for bullet
  -> AppMonad t (Dynamic t ServerBullet)
bullet i CreateBullet{..} = do
  syncBullet =<< simulateBullet initBullet
  where
    initBullet = Bullet {
        bulletVel = V2 createBulletVel createBulletVel * normalize createBulletDir
      , bulletPos = createBulletPos
      , bulletPlayer = createBulletPlayer
      , bulletLifeTime = 5
      , bulletCustom = ()
      }

    syncBullet bdyn = fmap join $ syncWithName (show i) bdyn $ do
      allPeers <- networkPeers
      let posSyncDt = 0.5  :: Double
      posSyncE <- tickEvery $ realToFrac posSyncDt
      _ <- syncToClientsManual allPeers bulletPosId ReliableMessage (bulletPos <$> bdyn) posSyncE
      _ <- syncToClients allPeers bulletVelId UnreliableMessage $ bulletVel <$> bdyn
      return bdyn

    simulateBullet Bullet{..} = do
      let bulletSymDt = 0.01 :: Double
          dtV = V2 bulletSymDt bulletSymDt
      tickE <- tickEvery $ realToFrac bulletSymDt
      posDyn <- foldDyn (const $ \p -> p + dtV * bulletVel) bulletPos tickE
      lifeDyn <- foldDyn (const $ \t -> t - bulletSymDt) bulletLifeTime tickE
      return $ Bullet
        <$> pure bulletVel
        <*> posDyn
        <*> pure bulletPlayer
        <*> lifeDyn
        <*> pure bulletCustom


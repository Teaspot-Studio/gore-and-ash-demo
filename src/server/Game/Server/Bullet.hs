module Game.Server.Bullet(
    processBullets
  , ServerBullet
  ) where

import Data.Map.Strict (Map)
import Data.Monoid
import Data.Store
import GHC.Generics
import Linear

import qualified Data.Map.Strict as M

import Game.GoreAndAsh
import Game.GoreAndAsh.Sync

import Game.Bullet
import Game.Monad
import Game.Player

-- | Info required to create new bullet
data CreateBullet = CreateBullet {
  createBulletPos    :: !(V2 Double) -- ^ Bullet spawn position
, createBulletDir    :: !(V2 Double) -- ^ Bullet flight direction
, createBulletPlayer :: !PlayerId    -- ^ Bullet owner
} deriving (Generic, Show)

instance Store CreateBullet

-- | Specific extended of bullet for server
type ServerBullet = Bullet ()

-- | Process all bullets
processBullets :: forall t . AppFrame t
  => Event t CreateBullet -- ^ Fires when a new bullet need to be spawned
  -> AppMonad t (Dynamic t (Map BulletId ServerBullet))
processBullets createE = do
  -- we need a bullet counter to generate ids
  bulletCounterRef <- newExternalRef (0 :: Int)
  bulletCounter <- externalRefDynamic bulletCounterRef
  -- define creation event that increses counter for ids
  addE <- performEvent $ ffor createE $ \ce -> do
    i <- sample . current $ bulletCounter
    modifyExternalRef bulletCounterRef $ \v -> (v+1, ())
    return $ M.singleton (BulletId i) (Just ce)
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
bullet _ CreateBullet{..} = return $ pure initBullet
  where
    bulletSpd = 10
    initBullet = Bullet {
        bulletVel = bulletSpd * normalize createBulletDir
      , bulletPos = createBulletPos
      , bulletPlayer = createBulletPlayer
      , bulletLifeTime = 5
      , bulletCustom = ()
      }
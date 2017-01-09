module Game(
    playGame
  , AppMonad
  , Game(..)
  ) where

import Data.Map.Strict (Map)
import Data.Monoid
import Data.Sequence (Seq)

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict.Merge as M

import Game.GoreAndAsh

import Game.Bullet
import Game.Global
import Game.Monad
import Game.Player
import Game.Server.Bullet
import Game.Server.Player


-- | Server game state
data Game = Game {
  gameGlobals :: GameGlobal
, gamePlayers :: PlayerMapping
, gameBullets :: Map BulletId ServerBullet
}

-- | Server logic
playGame :: AppFrame t => AppMonad t (Dynamic t Game)
playGame = do
  rec
    (players, shoots) <- playersCollection $ transHitMap bulets hits
    globals <- processGameGlobals
    (bulets, hits) <- processBullets (fmap fst players) (fmap transShoots shoots)
  return $ Game
    <$> globals
    <*> players
    <*> bulets

-- | Transform shoots request to flap sequence
transShoots :: Map PlayerId (Seq CreateBullet) -> Seq CreateBullet
transShoots = F.foldl' (<>) mempty

-- | Transform hit map for bullets to mapping from killed player to her killer
transHitMap :: forall t . AppFrame t
  => Dynamic t (Map BulletId ServerBullet) -- ^ Bullet mapping
  -> Event t (Map BulletId PlayerId) -- ^ Hit mapping
  -> Event t (Map PlayerId PlayerId) -- ^ Killed-killer mapping
transHitMap bulletMapDyn hitMapE = M.fromList . M.elems <$> taggedEvent
  where
    taggedEvent :: Event t (Map BulletId (PlayerId, PlayerId))
    taggedEvent = attachWith unionMappings (current bulletMapDyn) hitMapE
    unionMappings = M.merge M.dropMissing M.dropMissing $ M.zipWithMatched $ \_ b pid -> (pid, bulletPlayer b)

-- | Handle game globals
processGameGlobals :: AppFrame t => AppMonad t (Dynamic t GameGlobal)
processGameGlobals = return $ pure GameGlobal

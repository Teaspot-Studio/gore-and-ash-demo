module Game(
    playGame
  , AppMonad
  , Game(..)
  ) where

import Data.Map.Strict (Map)
import Data.Set (Set)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

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
    (players, shoots) <- playersCollection $ fmap transHitMap hits
    globals <- processGameGlobals
    (bulets, hits) <- processBullets (fmap fst players) shoots
  return $ Game
    <$> globals
    <*> players
    <*> bulets

-- | Transform hit map for bullets to set of hit players
transHitMap :: Map BulletId PlayerId -> Set PlayerId
transHitMap = S.fromList . M.elems

-- | Handle game globals
processGameGlobals :: AppFrame t => AppMonad t (Dynamic t GameGlobal)
processGameGlobals = return $ pure GameGlobal

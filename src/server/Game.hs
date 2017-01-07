module Game(
    playGame
  , AppMonad
  , Game(..)
  ) where

import Data.Map.Strict (Map)
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
  players <- playersCollection
  globals <- processGameGlobals
  bulets  <- processBullets never
  return $ Game
    <$> globals
    <*> players
    <*> bulets

-- | Handle game globals
processGameGlobals :: AppFrame t => AppMonad t (Dynamic t GameGlobal)
processGameGlobals = return $ pure GameGlobal

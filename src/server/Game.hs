module Game(
    playGame
  , AppMonad
  , Game(..)
  ) where

import Data.Map.Strict (Map)
import Game.GoreAndAsh

import Game.Global
import Game.Monad
import Game.Player
import Game.Server.Player

-- | Server game state
data Game = Game {
  gameGlobals :: GameGlobal
, gamePlayers :: PlayerMapping
}

-- | Server logic
playGame :: AppFrame t => AppMonad t (Dynamic t Game)
playGame = do
  players <- playersCollection
  globals <- processGameGlobals
  return $ Game
    <$> globals
    <*> players

-- | Handle game globals
processGameGlobals :: AppFrame t => AppMonad t (Dynamic t GameGlobal)
processGameGlobals = return $ pure GameGlobal

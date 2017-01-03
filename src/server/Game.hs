module Game(
    playGame
  , AppMonad
  , Game(..)
  ) where

import Game.GoreAndAsh

import Game.Global
import Game.Monad
import Game.Server.Player

-- | Server game state
data Game t = Game {
  gameGlobals :: Dynamic t GameGlobal
, gamePlayers :: Dynamic t (PlayerMapping t)
}

-- | Server logic
playGame :: AppFrame t => AppMonad t (Game t)
playGame = do
  globals <- processGameGlobals
  players <- playersCollection
  return Game {
      gameGlobals = globals
    , gamePlayers = players
    }

-- | Handle game globals
processGameGlobals :: AppFrame t => AppMonad t (Dynamic t GameGlobal)
processGameGlobals = return $ pure GameGlobal {
    gameScore = mempty
  }


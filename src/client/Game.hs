module Game(
    playGame
  , AppMonad
  , Game(..)
  ) where

import Data.Map.Strict (Map)

import Game.Client.Player
import Game.Global
import Game.Monad
import Game.Player

import Game.GoreAndAsh
import Game.GoreAndAsh.Sync

-- | Hold client-side state of game
data Game t = Game {
  gameGlobals :: Dynamic t GameGlobal
, gamePlayers :: Dynamic t (Map PlayerId (ClientPlayer t))
}

-- | Client logic
playGame :: AppFrame t => AppMonad t (Game t)
playGame = do
  globals <- receiveGlobals
  players <- handlePlayers
  return Game {
      gameGlobals = globals
    , gamePlayers = players
    }

-- | Get info about globals from server
receiveGlobals :: AppFrame t => AppMonad t (Dynamic t GameGlobal)
receiveGlobals = syncFromServer globalId $ GameGlobal mempty
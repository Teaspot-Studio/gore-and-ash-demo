module Game(
    playGame
  , AppMonad
  , Game(..)
  ) where

import Data.Map.Strict (Map)

import Game.Camera
import Game.Client.Player
import Game.Global
import Game.Monad
import Game.Player

import Game.GoreAndAsh
import Game.GoreAndAsh.SDL
import Game.GoreAndAsh.Sync

-- | Hold client-side state of game
data Game t = Game {
  gameGlobals :: Dynamic t GameGlobal
, gamePlayers :: Dynamic t (Map PlayerId (ClientPlayer t))
, gameCamera  :: Dynamic t Camera
}

-- | Client logic
playGame :: AppFrame t => WindowWidget t -> AppMonad t (Game t)
playGame w = do
  globals <- receiveGlobals
  players <- handlePlayers
  cam     <- camera w
  return Game {
      gameGlobals = globals
    , gamePlayers = players
    , gameCamera  = cam
    }

-- | Get info about globals from server
receiveGlobals :: AppFrame t => AppMonad t (Dynamic t GameGlobal)
receiveGlobals = syncFromServer globalId $ GameGlobal mempty
module Game(
    playGame
  , AppMonad
  , Game(..)
  ) where

import Game.Camera
import Game.Client.Player
import Game.Global
import Game.Monad

import Game.GoreAndAsh
import Game.GoreAndAsh.SDL
import Game.GoreAndAsh.Sync

-- | Hold client-side state of game
data Game = Game {
  -- | Global values of game (ex. score)
  gameGlobals     :: GameGlobal
  -- | Local player that is controlled by user
, gameLocalPlayer :: LocalPlayer
  -- | Other players that are controlled by other users
, gamePlayers     :: RemotePlayers
  -- | Local player camera
, gameCamera      :: Camera
}

-- | Client logic
playGame :: AppFrame t => WindowWidget t -- ^ Window where to draw game
  -> Bool -- ^ Cheating flag, simulate hacked client
  -> AppMonad t (Dynamic t Game)
playGame w cheating = do
  globals     <- receiveGlobals
  playersInfo <- handlePlayers w cheating
  cam         <- camera w
  return $ Game
    <$> globals
    <*> fmap fst playersInfo
    <*> fmap snd playersInfo
    <*> cam

-- | Get info about globals from server
receiveGlobals :: AppFrame t => AppMonad t (Dynamic t GameGlobal)
receiveGlobals = syncFromServer globalId $ GameGlobal mempty
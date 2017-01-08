module Game(
    playGame
  , AppMonad
  , Game(..)
  ) where

import Data.Map.Strict (Map)

import Game.Bullet
import Game.Camera
import Game.Client.Bullet
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
  -- | Known bullet map
, gameBullets     :: Map BulletId ClientBullet
  -- | Local player camera
, gameCamera      :: Camera
}

-- | Client logic
playGame :: AppFrame t => WindowWidget t -- ^ Window where to draw game
  -> Bool -- ^ Cheating flag, simulate hacked client
  -> AppMonad t (Dynamic t Game)
playGame w cheating = do
  globals     <- receiveGlobals
  cam         <- camera w
  playersInfo <- handlePlayers w cam cheating
  bullets     <- handleBullets
  return $ Game
    <$> globals
    <*> fmap fst playersInfo
    <*> fmap snd playersInfo
    <*> bullets
    <*> cam

-- | Get info about globals from server
receiveGlobals :: AppFrame t => AppMonad t (Dynamic t GameGlobal)
receiveGlobals = syncFromServer globalId GameGlobal
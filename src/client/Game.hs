module Game(
    playGame
  , AppMonad
  , Game(..)
  ) where

import Control.Monad 
import Data.Map.Strict (Map)

import Game.Bullet
import Game.Camera
import Game.Client.Bullet
import Game.Client.Player
import Game.Global
import Game.Monad
import Game.Player

import Game.GoreAndAsh
import Game.GoreAndAsh.Network
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
playGame :: forall t b m . (MonadGame t m, NetworkClient t b m, SyncMonad t b m)
  => Event t (WindowWidget t) -- ^ Window where to draw game
  -> Bool -- ^ Cheating flag, simulate hacked client
  -> m (Dynamic t Game)
playGame wE cheating = fmap join $ networkHold (pure $ pure initGame) $ ffor wE $ \w -> do
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
  where
    initGame = Game GameGlobal (Player 0 0 0 0 0 (PlayerId 0)) mempty mempty (Camera 0 0)

-- | Get info about globals from server
receiveGlobals :: forall t b m . (MonadGame t m, NetworkClient t b m, SyncMonad t b m) => m (Dynamic t GameGlobal)
receiveGlobals = syncFromServer globalId GameGlobal

module Game.Client.Player(
    handlePlayers
  , ClientPlayer
  , ClientPlayerExt(..)
  ) where

import Data.Map.Strict (Map)

import Game.GoreAndAsh
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Sync

import Game.Monad
import Game.Player

-- | Private client data about player
data ClientPlayerExt t = ClientPlayerExt {

  }

-- | Extended player with client data
type ClientPlayer t = Player t (ClientPlayerExt t)

-- | Sync players states from server
handlePlayers :: AppFrame t => AppMonad t (Dynamic t (Map PlayerId (ClientPlayer t)))
handlePlayers = do
  remoteCollection playerCollectionId player

-- | Client side controller for player
player :: AppFrame t => PlayerId -> Peer -> AppMonad t (ClientPlayer t)
player i _ = do
  syncPlayer
  where
    syncPlayer = syncWithName (show i) $ do
      pos <- syncFromServer playerPosId   0
      col <- syncFromServer playerColorId 0
      spd <- syncFromServer playerSpeedId 0
      siz <- syncFromServer playerSizeId  0
      return Player {
          playerPos   = pos
        , playerColor = col
        , playerSpeed = spd
        , playerSize  = siz
        , playerCustom = ClientPlayerExt {

          }
        }
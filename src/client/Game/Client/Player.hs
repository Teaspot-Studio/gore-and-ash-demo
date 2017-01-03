module Game.Client.Player(
    handlePlayers
  , ClientPlayer
  , ClientPlayerExt(..)
  ) where

import Data.Map.Strict (Map)

import Game.GoreAndAsh

import Game.Monad
import Game.Player

-- | Private client data about player
data ClientPlayerExt t = ClientPlayerExt {

  }

-- | Extended player with client data
type ClientPlayer t = Player t (ClientPlayerExt t)

-- | Sync players states from server
handlePlayers :: AppFrame t => AppMonad t (Dynamic t (Map PlayerId (ClientPlayer t)))
handlePlayers = return $ pure mempty
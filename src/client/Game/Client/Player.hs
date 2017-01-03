module Game.Client.Player(
    handlePlayers
  , ClientPlayer
  , ClientPlayerExt(..)
  ) where

import Control.Monad
import Data.Map.Strict (Map)
import Data.Monoid
import Linear

import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
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
  joinDynThroughMap <$> remoteCollection playerCollectionId player

-- | Client side controller for player
player :: forall t . AppFrame t => PlayerId -> Peer -> AppMonad t (Dynamic t (ClientPlayer t))
player i _ = do
  buildE <- getPostBuild
  logInfoE $ ffor buildE $ const $ "Player " <> showl i <> " is created!"
  p <- syncPlayer
  printPlayer p
  return p
  where
    initialPlayer = Player {
        playerPos    = 0
      , playerColor  = V3 1 0 0
      , playerSpeed  = 50
      , playerSize   = 20
      , playerCustom = ClientPlayerExt
      }

    syncPlayer :: AppMonad t (Dynamic t (ClientPlayer t))
    syncPlayer = fmap join $ syncWithName (show i) (pure initialPlayer) $ do
      pos <- syncFromServer playerPosId   0
      col <- syncFromServer playerColorId 0
      spd <- syncFromServer playerSpeedId 0
      siz <- syncFromServer playerSizeId  0
      return $ Player
        <$> pos
        <*> col
        <*> spd
        <*> siz
        <*> pure ClientPlayerExt

    printPlayer :: Dynamic t (ClientPlayer t) -> AppMonad t ()
    printPlayer pdyn =
      logInfoE $ ffor (updated pdyn) $ \Player{..} -> "Player:\n"
        <> "\tid:    " <> showl i   <> "\n"
        <> "\tpos:   " <> showl playerPos <> "\n"
        <> "\tcolor: " <> showl playerColor <> "\n"
        <> "\tspd:   " <> showl playerSpeed <> "\n"
        <> "\tsize:  " <> showl playerSize
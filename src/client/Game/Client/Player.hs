module Game.Client.Player(
    handlePlayers
  , ClientPlayer
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

-- | Extended player with client data
type ClientPlayer = Player ()

-- | Sync players states from server
handlePlayers :: AppFrame t => AppMonad t (Dynamic t (Map PlayerId ClientPlayer))
handlePlayers = do
  joinDynThroughMap <$> remoteCollection playerCollectionId player

-- | Client side controller for personal player
-- localPlayer :: forall t . AppFrame t => AppMonad t (Dynamic t ClientPlayer)
-- localPlayer = undefined

-- | Client side controller for player
player :: forall t . AppFrame t => PlayerId -> Peer -> AppMonad t (Dynamic t ClientPlayer)
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
      , playerCustom = ()
      }

    syncPlayer :: AppMonad t (Dynamic t ClientPlayer)
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
        <*> pure ()

    printPlayer :: Dynamic t ClientPlayer -> AppMonad t ()
    printPlayer pdyn =
      logInfoE $ ffor (updated pdyn) $ \Player{..} -> "Player:\n"
        <> "\tid:    " <> showl i   <> "\n"
        <> "\tpos:   " <> showl playerPos <> "\n"
        <> "\tcolor: " <> showl playerColor <> "\n"
        <> "\tspd:   " <> showl playerSpeed <> "\n"
        <> "\tsize:  " <> showl playerSize
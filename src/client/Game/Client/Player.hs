module Game.Client.Player(
    handlePlayers
  , ClientPlayer
  , RemotePlayers
  , LocalPlayer
  ) where

import Control.Monad
import Data.Map.Strict (Map)
import Data.Monoid
import Linear

import qualified Data.Map.Strict as M

import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Sync
import Game.GoreAndAsh.Time

import Game.Monad
import Game.Player

-- | Extended player with client data
type ClientPlayer = Player ()

-- | Collection for other remote players
type RemotePlayers = Map PlayerId RemotePlayer

-- | Remote player that is controlled by server
type RemotePlayer = ClientPlayer

-- | Local player that is controlled by local user
type LocalPlayer = ClientPlayer

-- | Sync players states from server
handlePlayers :: forall t . AppFrame t => AppMonad t (Dynamic t (LocalPlayer, RemotePlayers))
handlePlayers = do
  -- unify phases that they have equal result types
  let
    phase1Unified :: AppMonad t (Event t PlayerId, Dynamic t (LocalPlayer, RemotePlayers))
    phase1Unified = do
      e <- phase1
      return (e, pure (initialPlayer, mempty))

    phase2Unified :: PlayerId -> AppMonad t (Event t PlayerId, Dynamic t (LocalPlayer, RemotePlayers))
    phase2Unified localId = do
      pair <- phase2 localId
      return (never, pair)
  rec
    res <- holdAppHost phase1Unified $ phase2Unified <$> localIdE
    let localIdE = switchPromptlyDyn . fmap fst $ res
  return $ join . fmap snd $ res
  where
    -- Before we cant load other players we need to know id of local player.
    -- Server should send message about it immidieately after creation of new player.
    phase1 :: AppMonad t (Event t PlayerId)
    phase1 = do
      buildE <- getPostBuild
      -- listen to response from server
      msgE <- receiveFromServer playerCommandId
      let localIdE = fforMaybe msgE $ \case
            YourPlayerId i -> Just i
            _ -> Nothing
      -- send request periodically
      let stopTickE = const () <$> localIdE -- stop ticking as soon as got id
      tickE <- tickEveryUntil (realToFrac (2 :: Double)) stopTickE
      let reqE = const RequestPlayerId <$> leftmost [tickE, buildE]
      _ <- sendToServer playerCommandId ReliableMessage reqE
      -- external switch as soon as got id
      return localIdE

    -- Now when we have local player id, we can load remote players
    phase2 :: PlayerId -> AppMonad t (Dynamic t (LocalPlayer, RemotePlayers))
    phase2 localId = do
      lplayer <- localPlayer localId
      remotePlayers <- joinDynThroughMap <$> remoteCollection playerCollectionId (player localId)
      return $ (,)
        <$> lplayer
        <*> (M.delete localId <$> remotePlayers)

-- | Default value for client player
initialPlayer :: ClientPlayer
initialPlayer = Player {
    playerPos    = 0
  , playerColor  = V3 1 0 0
  , playerSpeed  = 50
  , playerSize   = 20
  , playerCustom = ()
  }

-- | Display info about player
printPlayer :: AppFrame t => PlayerId -> Dynamic t ClientPlayer -> AppMonad t ()
printPlayer i pdyn =
  logInfoE $ ffor (updated pdyn) $ \Player{..} -> "Player:\n"
    <> "\tid:    " <> showl i   <> "\n"
    <> "\tpos:   " <> showl playerPos <> "\n"
    <> "\tcolor: " <> showl playerColor <> "\n"
    <> "\tspd:   " <> showl playerSpeed <> "\n"
    <> "\tsize:  " <> showl playerSize

-- | Client side controller for personal player
localPlayer :: forall t . AppFrame t => PlayerId -> AppMonad t (Dynamic t LocalPlayer)
localPlayer i = do
  buildE <- getPostBuild
  logInfoE $ ffor buildE $ const $ "Local player " <> showl i <> " is created!"
  p <- syncPlayer
  printPlayer i p
  return p
  where
    syncPlayer :: AppMonad t (Dynamic t LocalPlayer)
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

-- | Client side controller for player
player :: forall t . AppFrame t => PlayerId -- ^ Local id that indicates that controller should do nothing
  -> PlayerId -> Peer -> AppMonad t (Dynamic t ClientPlayer)
player localId i _ = if i == localId then return $ pure initialPlayer
  else do
    buildE <- getPostBuild
    logInfoE $ ffor buildE $ const $ "Player " <> showl i <> " is created!"
    p <- syncPlayer
    printPlayer i p
    return p
  where
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
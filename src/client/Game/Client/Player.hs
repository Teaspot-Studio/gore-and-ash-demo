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
import Game.GoreAndAsh.SDL hiding (delay)
import Game.GoreAndAsh.Sync
import Game.GoreAndAsh.Time

import Game.Camera
import Game.Player

-- | Extended player with client data
type ClientPlayer = Player ()

-- | Collection for other remote players
type RemotePlayers = Map PlayerId RemotePlayer

-- | Remote player that is controlled by server
type RemotePlayer = ClientPlayer

-- | Local player that is controlled by local user
type LocalPlayer = Player PlayerId

-- | Sync players states from server
handlePlayers :: forall t m b . (MonadGame t m, SyncMonad t b m, NetworkClient t b m)
  => WindowWidget t -- ^ Window for player inputs
  -> Dynamic t Camera -- ^ Camera for detecting world position of clicks
  -> Bool -- ^ Cheating flag, simulate hacked client
  -> m (Dynamic t (LocalPlayer, RemotePlayers))
handlePlayers w camDyn cheating = do
  -- unify phases that they have equal result types
  let
    phase1Unified :: m (Event t PlayerId, Dynamic t (LocalPlayer, RemotePlayers))
    phase1Unified = do
      e <- phase1
      return (e, pure (initialPlayer { playerCustom = PlayerId 0 }, mempty))

    phase2Unified :: PlayerId -> m (Event t PlayerId, Dynamic t (LocalPlayer, RemotePlayers))
    phase2Unified localId = do
      pair <- phase2 localId
      return (never, pair)
  rec
    res <- networkHold phase1Unified $ phase2Unified <$> localIdE
    let localIdE = switch . current . fmap fst $ res
  return $ join . fmap snd $ res
  where
    -- Before we cant load other players we need to know id of local player.
    -- Server should send message about it immidieately after creation of new player.
    phase1 :: m (Event t PlayerId)
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
    phase2 :: PlayerId -> m (Dynamic t (LocalPlayer, RemotePlayers))
    phase2 localId = do
      lplayer <- localPlayer w camDyn localId cheating
      (remotePlayers', _) <- remoteCollection playerCollectionId (\pid () -> player localId pid)
      let remotePlayers = joinDynThroughMap remotePlayers'
      return $ (,)
        <$> lplayer
        <*> (M.delete localId <$> remotePlayers)

-- | Default value for client player
initialPlayer :: ClientPlayer
initialPlayer = Player {
    playerPos    = 0
  , playerColor  = V3 1 0 0
  , playerSpeed  = 0
  , playerSize   = 0
  , playerScore  = 0
  , playerCustom = ()
  }

-- | Client side controller for personal player
localPlayer :: forall t b m . (MonadGame t m, LoggingMonad t m, SyncMonad t b m, NetworkClient t b m)
  => WindowWidget t -- ^ Window the player inputs are came from
  -> Dynamic t Camera -- ^ Camera for detecting world positions
  -> PlayerId -- ^ ID of local player
  -> Bool -- ^ Cheating flag, simulate hacked client
  -> m (Dynamic t LocalPlayer)
localPlayer w camDyn i cheating = do
  buildE <- getPostBuild
  logInfoE $ ffor buildE $ const $ "Local player " <> showl i <> " is created!"
  p <- syncPlayer
  -- printPlayer i p
  shootE <- shoot p
  processCommands shootE
  return $ fmap (const i) <$> p
  where
    syncPlayer :: m (Dynamic t ClientPlayer)
    syncPlayer = fmap join $ syncWithName (show i) (pure initialPlayer) $ do
      col <- syncFromServer playerColorId 0
      spd <- fmap (if cheating then (*2) else id) <$> syncFromServer playerSpeedId 0
      pos <- syncPosition spd
      siz <- syncFromServer playerSizeId  0
      score <- syncFromServer playerScoreId 0
      return $ Player
        <$> pos
        <*> col
        <*> spd
        <*> siz
        <*> score
        <*> pure ()

    -- | Generate events when user wants to move player
    movePlayer :: Dynamic t Double -> m (Event t (V2 Double))
    movePlayer spdDyn = do
      -- generate press event each dt seconds
      let dt = 0.01 :: Double
      tickE <- fmap (const dt) <$> tickEvery (realToFrac dt)
      let
        dvDown  = V2 0    (-1)
        dvUp    = V2 0    1
        dvLeft  = V2 (-1) 0
        dvRight = V2 1    0
      -- detect movement in directions
      downE  <- fmap (dvDown  *) <$> pressingEvent tickE ScancodeDown
      upE    <- fmap (dvUp    *) <$> pressingEvent tickE ScancodeUp
      leftE  <- fmap (dvLeft  *) <$> pressingEvent tickE ScancodeLeft
      rightE <- fmap (dvRight *) <$> pressingEvent tickE ScancodeRight
      -- collect them by sum
      let moveE = mergeWith (+) [downE, upE, leftE, rightE]
      -- multiply with current speed value
      let spdDyn' = (\v -> V2 v v) <$> spdDyn
      return $ attachWith (*) (current spdDyn') moveE

    -- While key pressed generate the following event
    pressingEvent :: Event t Double -> Scancode -> m (Event t (V2 Double))
    pressingEvent e scode = do
      pressDyn <- keyPressing w scode
      let mkTag mpress v = const (V2 v v) <$> mpress
      return $ attachWithMaybe mkTag (current pressDyn) e

    -- | Synchronisation of position with rejections from server
    syncPosition :: Dynamic t Double -> m (Dynamic t (V2 Double))
    syncPosition spdDyn = do
      moveE <- movePlayer spdDyn
      rec
        serverE <- syncToServer playerPosId ReliableMessage positionDyn
        let rejectE = serverRejected serverE
            userE = attachWith (+) (current positionDyn) moveE
        positionDyn <- holdDyn 0 $ leftmost [rejectE, userE]
      return $ positionDyn

    shoot :: Dynamic t ClientPlayer -> m (Event t (V2 Double))
    shoot pDyn = do
      clickE <- mouseClickPress w ButtonLeft playerShootRatio
      return $ switch . current $ do
        p <- pDyn
        cam <- camDyn
        return $ ffor clickE $ \wp -> normalize $ cameraToWorld cam wp - playerPos p

    -- Send commands to server
    processCommands :: Event t (V2 Double) -> m ()
    processCommands shootE = do
      _ <- sendToServer playerCommandId ReliableMessage $ PlayerShoot <$> shootE
      return ()

-- | Client side controller for player
player :: forall t b m . (MonadGame t m, LoggingMonad t m, SyncMonad t b m, NetworkClient t b m)
  => PlayerId -- ^ Local id that indicates that controller should do nothing
  -> PlayerId -- ^ ID of player the controller should create, if matches with first parameter, do nothing
  -> m (Dynamic t ClientPlayer)
player localId i = if i == localId then return $ pure initialPlayer
  else do
    buildE <- getPostBuild
    logInfoE $ ffor buildE $ const $ "Player " <> showl i <> " is created!"
    p <- syncPlayer
    -- printPlayer i p
    return p
  where
    syncPlayer :: m (Dynamic t ClientPlayer)
    syncPlayer = fmap join $ syncWithName (show i) (pure initialPlayer) $ do
      let interpT = realToFrac (0.1 :: Double) -- not too fast, not too slow
          interpSteps = 10 -- five steps are enough for smooth transition
      pos <- linearInterpolate interpSteps interpT =<< syncFromServer playerPosId 0
      col <- syncFromServer playerColorId 0
      spd <- syncFromServer playerSpeedId 0
      siz <- syncFromServer playerSizeId  0
      score <- syncFromServer playerScoreId  0
      return $ Player
        <$> pos
        <*> col
        <*> spd
        <*> siz
        <*> score
        <*> pure ()

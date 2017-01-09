module Game.Server.Player(
    PlayerMapping
  , playersCollection
  , ServerPlayer
  , ServerPlayerExt(..)
  , PlayerShoots
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Align
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Sequence (Seq)
import Data.These
import Linear

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Sync

import Game.Bullet
import Game.Monad
import Game.Player

-- | Contains mappings between player ids, peers and player payload
type PlayerMapping = (Map PlayerId ServerPlayer, Map Peer PlayerId)

-- | Requests from players to create bullets
type PlayerShoots t = Event t (Map PlayerId (Seq CreateBullet))

-- | Shared players collection
playersCollection :: forall t . AppFrame t
  => Event t (Map PlayerId PlayerId) -- ^ Fires when a hit from bullets are occured. (killed => killer)
  -> AppMonad t (Dynamic t PlayerMapping, PlayerShoots t)
playersCollection hitsE = do
  -- we need a player counter to generate ids
  playerCounterRef <- newExternalRef (0 :: Int)
  playerCounter <- externalRefDynamic playerCounterRef
  -- retreive necessary events
  connPeerE <- peerConnected
  discPeerE <- peerDisconnected
  rec
    -- Lets calculate update actions for players collection
    -- updE :: Event t (Map PlayerId (Maybe Peer))
    updE <- performEvent $ ffor (align connPeerE discPeerE) $ \e -> do
      i <- sample . current $ playerCounter
      case e of
        This connPeer -> do
          modifyExternalRef playerCounterRef $ \v -> (v+1, ())
          return $ M.singleton (PlayerId i) (Just connPeer)
        That discPeer -> do
          players <- sample . current $ fmap snd playersMappingDyn
          return $ case M.lookup discPeer players of
            Nothing -> mempty
            Just i' -> M.singleton i' Nothing
        These connPeer discPeer -> if connPeer == discPeer then return mempty
          else do
            modifyExternalRef playerCounterRef $ \v -> (v+1, ())
            players <- sample . current $ fmap snd playersMappingDyn
            let
              delMap :: Map PlayerId (Maybe Peer)
              delMap = case M.lookup discPeer players of
                Nothing -> mempty
                Just i' -> M.singleton i' Nothing
            return $ M.insert (PlayerId i) (Just connPeer) delMap
    -- collection primitive, note recursive dependency
    colorRoller <- makeColorRoller
    let playerWrapper i = player colorRoller (mkHitE i) (mkKills i) i
    collReses <- hostSimpleCollection playerCollectionId mempty updE playerWrapper
    let playersMapDyn = joinDynThroughMap $ fmap fst <$> collReses
        shootsEvents  = switchPromptlyDyn $ mergeMap . fmap snd <$> collReses
    -- post processing to get peer-id map
    let playersMappingDyn :: Dynamic t PlayerMapping
        playersMappingDyn = do
          playersMap <- playersMapDyn
          let elems = M.toList $ playerPeer . playerCustom <$> playersMap
              peersMap = M.fromList . fmap (\(i, p) -> (p, i)) $ elems
          return (playersMap, peersMap)

  return (playersMappingDyn, shootsEvents)
  where
    -- Construct event that particular player is hit by a bullet
    mkHitE i = fforMaybe hitsE $ \m -> if M.member i m then Just () else Nothing
    -- Construct event that particular player killed another player
    mkKills i = fforMaybe hitsE $ \m -> let n = length . filter (== i) . M.elems $ m
      in if n > 0 then Just n else Nothing

-- | Extension of shared player with server private data
type ServerPlayer = Player ServerPlayerExt

-- | Player server private data
data ServerPlayerExt = ServerPlayerExt {
  playerPeer :: Peer
} deriving (Show)

-- | Player component
player :: forall t . AppFrame t
  => ItemRoller t (V3 Double) -- ^ Roller of colors
  -> Event t () -- ^ Hit event from bullet
  -> Event t Int -- ^ Event about count of murders of another players
  -> PlayerId -- ^ Player ID that is simulated
  -> Peer -- ^ Player peer
  -- | Returns dynamic player and event when user requests bullet creation
  -> AppMonad t (Dynamic t ServerPlayer, Event t (Seq CreateBullet))
player colorRoller hitE killsE i peer = do
  -- Initialisation
  buildE <- getPostBuild
  logInfoE $ ffor buildE $ const $ "Player " <> showl i <> " is spawned!"
  _ <- performEvent_ $ ffor buildE $ const $ liftIO $ snd colorRoller
  -- Local simulation of player (creation and score acummulation)
  c <- sample . current $ fst colorRoller
  playerDyn <- simulatePlayer $ initialPlayer c
  -- Sync player with clients
  playerDyn' <- syncPlayer playerDyn
  -- Process commands for client-server communication
  let yourIdMsgE = ffor buildE $ const [YourPlayerId i]
  let commandsE = yourIdMsgE
  shootsE <- syncCommands commandsE playerDyn'
  -- Print and return state and control events
  -- printPlayer i playerDyn'
  return (playerDyn', shootsE)
  where
    initialPlayer c = Player {
        playerPos    = initialPosition
      , playerColor  = c
      , playerSpeed  = 50
      , playerSize   = 5
      , playerScore  = 0
      , playerCustom = ServerPlayerExt {
          playerPeer = peer
        }
      }
    initialPosition = V2 0 0

    -- | Reactimate player with local server logic.
    simulatePlayer :: ServerPlayer -> AppMonad t (Dynamic t ServerPlayer)
    simulatePlayer Player{..} = do
      score <- collectPlayerScore
      return $ Player
        <$> pure playerPos
        <*> pure playerColor
        <*> pure playerSpeed
        <*> pure playerSize
        <*> score
        <*> pure playerCustom

    -- | Collect events about player murders
    collectPlayerScore :: AppMonad t (Dynamic t Int)
    collectPlayerScore = foldDyn (+) 0 killsE

    -- synchronisation of state
    syncPlayer :: Dynamic t ServerPlayer -> AppMonad t (Dynamic t ServerPlayer)
    syncPlayer pdyn = fmap join $ syncWithName (show i) pdyn $ do
      allPeers <- networkPeers
      let otherPeers = S.delete peer <$> allPeers
      posDyn <- syncPosition $ playerSpeed <$> pdyn
      _ <- syncToClients otherPeers playerPosId ReliableMessage posDyn -- TODO: unreliable messages are dropped off on high load
      _ <- syncToClients allPeers playerColorId ReliableMessage $ playerColor <$> pdyn
      _ <- syncToClients allPeers playerSpeedId ReliableMessage $ playerSpeed <$> pdyn
      _ <- syncToClients allPeers playerSizeId  ReliableMessage $ playerSize <$> pdyn
      _ <- syncToClients allPeers playerScoreId ReliableMessage $ playerScore <$> pdyn
      return $ do
        pos <- posDyn
        p <- pdyn
        return $ p { playerPos = pos }

    -- Synchronise position from client with rejecting if player moves too fast
    syncPosition :: Dynamic t Double -> AppMonad t (Dynamic t (V2 Double))
    syncPosition spdDyn = do
      let dt = 0.5 :: Double -- ^ Interval between cheats check
          epsylon = 0.1 :: Double -- ^ Accuracy of checking
      rec
        oldPosDyn <- lookPast (realToFrac dt) initialPosition rejectE posDyn
        let posRejectE = flip push (updated oldPosDyn) $ \oldPos -> do
              pos <- sample . current $ posDyn
              spd <- sample . current $ spdDyn
              let absSpeed = norm (pos - oldPos) / dt
                  expectedSpeed = spd * sqrt 2 * (1 + epsylon) -- diagonal movement
              return $ if absSpeed > expectedSpeed
                then Just oldPos
                else Nothing
            respawnE = ffor hitE $ const initialPosition
            rejectE = leftmost [respawnE, posRejectE]
        (posDyn, _) <- syncFromClient playerPosId (return $ pure 0) rejectE peer
      return posDyn

    -- process network messages for player
    syncCommands commandsE playerDyn = do
      -- listen requests for id
      msgsE <- receiveFromClient playerCommandId peer
      let respE = fmapMaybe seqLeftMay $ fforSeqMaybe msgsE $ \case
            RequestPlayerId -> Just [YourPlayerId i]
            _ -> Nothing
          shootsE = pushSeq msgsE $ \case
            PlayerShoot v -> do
              Player{..} <- sample . current $ playerDyn
              let dpos = (realToFrac $ playerSize * 1.5) * normalize v
              return $ Just CreateBullet {
                  createBulletPos = playerPos + dpos
                , createBulletDir = v
                , createBulletPlayer = i
                , createBulletVel = 50
                }
            _ -> return Nothing
      -- send commands/responses to peer
      _ <- sendToClientMany playerCommandId ReliableMessage (commandsE <> respE) peer
      return shootsE

-- | Create item roller for player colors
makeColorRoller :: AppFrame t => AppMonad t (ItemRoller t (V3 Double))
makeColorRoller = itemRoller $ NE.fromList [
    V3 1 0 0
  , V3 0 1 0
  , V3 0 0 1
  , V3 1 1 0
  , V3 1 0 1
  , V3 0 1 1
  ]

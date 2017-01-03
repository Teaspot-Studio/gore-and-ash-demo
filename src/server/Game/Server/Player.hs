module Game.Server.Player(
    PlayerMapping
  , playersCollection
  , ServerPlayer
  , ServerPlayerExt(..)
  ) where

import Control.Monad.IO.Class
import Data.Align
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.These
import Linear

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M

import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Sync

import Game.Monad
import Game.Player

-- | Contains mappings between player ids, peers and player payload
type PlayerMapping = (Map PlayerId ServerPlayer, Map Peer PlayerId)

-- | Shared players collection
playersCollection :: forall t . AppFrame t => AppMonad t (Dynamic t PlayerMapping)
playersCollection = do
  -- we need a player counter to generate ids
  playerCounterRef <- newExternalRef (0 :: Int)
  playerCounter <- externalRefDynamic playerCounterRef
  -- retreive necessary events
  connPeerE <- peerConnected
  discPeerE <- peerDisconnected
  rec
    -- Lets calculate update actions for players collection
    let
      updE :: Event t (Map PlayerId (Maybe Peer))
      updE = flip pushAlways (align connPeerE discPeerE) $ \e -> do
        i <- sample . current $ playerCounter
        case e of
          This connPeer -> return $ M.singleton (PlayerId i) (Just connPeer)
          That discPeer -> do
            players <- sample . current $ fmap snd playersMappingDyn
            return $ case M.lookup discPeer players of
              Nothing -> mempty
              Just i' -> M.singleton i' Nothing
          These connPeer discPeer -> if connPeer == discPeer then return mempty
            else do
              players <- sample . current $ fmap snd playersMappingDyn
              let
                delMap :: Map PlayerId (Maybe Peer)
                delMap = case M.lookup discPeer players of
                  Nothing -> mempty
                  Just i' -> M.singleton i' Nothing
              return $ M.insert (PlayerId i) (Just connPeer) delMap
    -- collection primitive, note recursive dependency
    colorRoller <- makeColorRoller
    playersMapDyn <- joinDynThroughMap <$> hostSimpleCollection playerCollectionId mempty updE (player colorRoller)
    -- post processing to get peer-id map
    let playersMappingDyn :: Dynamic t PlayerMapping
        playersMappingDyn = do
          playersMap <- playersMapDyn
          let elems = M.toList $ playerPeer . playerCustom <$> playersMap
              peersMap = M.fromList . fmap (\(i, p) -> (p, i)) $ elems
          return (playersMap, peersMap)

  return playersMappingDyn

-- | Extension of shared player with server private data
type ServerPlayer = Player ServerPlayerExt

-- | Player server private data
data ServerPlayerExt = ServerPlayerExt {
  playerPeer :: Peer
}

-- | Player component
player :: AppFrame t => ItemRoller t (V3 Double) -- ^ Roller of colors
  -> PlayerId -- ^ Player ID that is simulated
  -> Peer -- ^ Player peer
  -> AppMonad t (Dynamic t ServerPlayer)
player colorRoller i peer = do
  -- Initialisation
  buildE <- getPostBuild
  logInfoE $ ffor buildE $ const $ "Player " <> showl i <> " is spawned!"
  _ <- performEvent_ $ ffor buildE $ const $ liftIO $ snd colorRoller
  -- Sync player with clients
  c <- sample . current $ fst colorRoller
  let playerDyn = pure $ initialPlayer c

  let yourIdMsgE = ffor buildE $ const [YourPlayerId i]
  let commandsE = yourIdMsgE
  _ <- syncPlayer commandsE playerDyn
  return playerDyn
  where
    initialPlayer c = Player {
        playerPos    = 0
      , playerColor  = c
      , playerSpeed  = 1
      , playerSize   = 5
      , playerCustom = ServerPlayerExt {
          playerPeer = peer
        }
      }
    syncPlayer commandsE pdyn = syncWithName (show i) () $ do
      peers <- networkPeers
      _ <- syncToClients peers playerPosId   UnreliableMessage $ playerPos <$> pdyn
      _ <- syncToClients peers playerColorId ReliableMessage $ playerColor <$> pdyn
      _ <- syncToClients peers playerSpeedId ReliableMessage $ playerSpeed <$> pdyn
      _ <- syncToClients peers playerSizeId  ReliableMessage $ playerSize <$> pdyn
      _ <- sendToClientMany playerCommandId ReliableMessage commandsE peer
      return ()

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

-- | Item roller contains dynamic of current element and action to switch to next
-- item
type ItemRoller t a = (Dynamic t a, IO ())

-- | Create a item chooser from given list, returns dynamic with current item
-- and action to change it.
itemRoller :: AppFrame t => NonEmpty a -> AppMonad t (ItemRoller t a)
itemRoller as = do
  ref <- newExternalRef (NE.toList as, [])
  let getCurItem xs = case xs of
        [] -> error "itemRoller: impossible"
        (x : _) -> x
  curDyn <- fmap (getCurItem . fst) <$> externalRefDynamic ref
  let updRoller = modifyExternalRef ref $ \(xs, ys) -> case xs of
        []        -> ((reverse ys, []),  ())
        [x]       -> ((reverse ys, [x]), ())
        (x : xs') -> ((xs', x : ys), ())
  return (curDyn, updRoller)

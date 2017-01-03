module Game.Server.Player(
    PlayerMapping
  , playersCollection
  , ServerPlayer
  , ServerPlayerExt(..)
  ) where

import Data.Map.Strict (Map)
import Data.Align
import Data.These

import qualified Data.Map.Strict as M

import Game.GoreAndAsh
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Sync

import Game.Monad
import Game.Player

-- | Contains mappings between player ids, peers and player payload
type PlayerMapping t = (Map PlayerId (ServerPlayer t), Map Peer PlayerId)

-- | Shared players collection
playersCollection :: forall t . AppFrame t => AppMonad t (Dynamic t (PlayerMapping t))
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
    playersMapDyn <- hostSimpleCollection playerCollectionId mempty updE player
    -- post processing to get peer-id map
    let playersMappingDyn :: Dynamic t (PlayerMapping t)
        playersMappingDyn = do
          playersMap <- playersMapDyn
          let elems = M.toList $ playerPeer . playerCustom <$> playersMap
              peersMap = M.fromList . fmap (\(i, p) -> (p, i)) $ elems
          return (playersMap, peersMap)
  return playersMappingDyn

-- | Extension of shared player with server private data
type ServerPlayer t  = Player t (ServerPlayerExt t)

-- | Player server private data
data ServerPlayerExt t = ServerPlayerExt {
  playerPeer :: Peer
}

-- | Player component
player :: PlayerId -> Peer -> AppMonad t (ServerPlayer t)
player _ _ = undefined
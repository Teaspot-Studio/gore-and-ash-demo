module Game.Monad(
    AppMonad
  , AppPeer
  , AppNetworkBackend
  , MonadApp
  ) where

import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Network.Backend.TCP
import Game.GoreAndAsh.Sync

-- | Which network implementation to use
type AppNetworkBackend = TCPBackend

-- | Peer connection for application monad
type AppPeer = Peer AppNetworkBackend

-- | Application monad that is used for implementation of game API
type AppMonad a = SyncT Spider AppNetworkBackend (NetworkT Spider AppNetworkBackend (LoggingT Spider GMSpider))

-- | Shortcut for constraints
type MonadApp t m = (MonadGame t m, NetworkServer t AppNetworkBackend m, NetworkClient t AppNetworkBackend m, SyncMonad t AppNetworkBackend m)

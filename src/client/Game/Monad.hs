module Game.Monad(
    AppMonad
  , AppPeer
  ) where

import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Network.Backend.TCP
import Game.GoreAndAsh.SDL
import Game.GoreAndAsh.Sync

-- | Which network implementation to use
type AppNetworkBackend = TCPBackend

-- | Peer connection for application monad
type AppPeer = Peer AppNetworkBackend

-- | Application monad that is used for implementation of game API
type AppMonad a = SDLT Spider (SyncT Spider AppNetworkBackend (NetworkT Spider AppNetworkBackend (LoggingT Spider GMSpider)))

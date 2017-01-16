{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Game.Monad(
    AppMonad(..)
  , AppFrame
  , AppPeer
  ) where

import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Proxy

import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Network.Backend.TCP
import Game.GoreAndAsh.Sync
import Game.GoreAndAsh.Time

-- | Which network implementation to use
type AppNetworkBackend = TCPBackend

-- | Helper for concise type class contexts
type AppFrame t = (ReflexHost t, MonadIO (HostFrame t), MonadMask (HostFrame t), MonadBaseControl IO (HostFrame t), MonadIO (HostFrame t))

-- | Application monad that is used for implementation of game API
type AppStack t = SyncT t AppNetworkBackend (TimerT t (NetworkT t AppNetworkBackend (LoggingT t (GameMonad t))))

-- | Peer connection for application monad
type AppPeer = Peer AppNetworkBackend

-- | Wrapper around 'AppStack'
newtype AppMonad t a = AppMonad { runAppMonad :: AppStack t a}
  deriving (Functor, Applicative, Monad, MonadFix)

deriving instance (ReflexHost t, MonadCatch (HostFrame t)) => MonadCatch (AppMonad t)
deriving instance (ReflexHost t, MonadThrow (HostFrame t)) => MonadThrow (AppMonad t)
deriving instance (ReflexHost t, MonadMask (HostFrame t)) => MonadMask (AppMonad t)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => MonadIO (AppMonad t)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => LoggingMonad t (AppMonad t)
deriving instance (ReflexHost t) => MonadSample t (AppMonad t)
deriving instance (ReflexHost t) => MonadHold t (AppMonad t)
deriving instance (ReflexHost t) => MonadSubscribeEvent t (AppMonad t)
deriving instance (ReflexHost t, MonadCatch (HostFrame t), MonadBaseControl IO (HostFrame t), MonadIO (HostFrame t)) => NetworkMonad t AppNetworkBackend (AppMonad t)
deriving instance (ReflexHost t, MonadCatch (HostFrame t), MonadBaseControl IO (HostFrame t), MonadIO (HostFrame t)) => NetworkClient t AppNetworkBackend (AppMonad t)
deriving instance (ReflexHost t, MonadCatch (HostFrame t), MonadBaseControl IO (HostFrame t), MonadIO (HostFrame t)) => NetworkServer t AppNetworkBackend (AppMonad t)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => TimerMonad t (AppMonad t)
deriving instance (ReflexHost t, MonadMask (HostFrame t), MonadIO (HostFrame t)) => SyncMonad t AppNetworkBackend (AppMonad t)

instance ReflexHost t => MonadReflexCreateTrigger t (AppMonad t) where
  newEventWithTrigger = AppMonad . newEventWithTrigger
  newFanEventWithTrigger trigger = AppMonad $ newFanEventWithTrigger trigger

instance (ReflexHost t, MonadIO (HostFrame t), MonadMask (HostFrame t), MonadBaseControl IO (HostFrame t)) => GameModule t (AppMonad t) where
  type ModuleOptions t (AppMonad t) = SyncOptions (NetworkOptions () AppNetworkBackend)
  runModule opts m = runModule opts $ runAppMonad m
  withModule t _ = withModule t (Proxy :: Proxy (AppStack t))

instance (ReflexHost t, MonadIO (HostFrame t)) => MonadAppHost t (AppMonad t) where
  getFireAsync = AppMonad getFireAsync
  getRunAppHost = do
    runner <- AppMonad getRunAppHost
    return $ \m -> runner $ runAppMonad m
  performPostBuild_ = AppMonad . performPostBuild_
  liftHostFrame = AppMonad . liftHostFrame

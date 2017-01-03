{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Game.Monad(
    AppMonad(..)
  , AppFrame
  ) where

import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Proxy

import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.SDL
import Game.GoreAndAsh.Sync
import Game.GoreAndAsh.Time

-- | Helper for concise type class contexts
type AppFrame t = (ReflexHost t, MonadIO (HostFrame t), MonadMask (HostFrame t), MonadBaseControl IO (HostFrame t), MonadIO (HostFrame t))

-- | Application monad that is used for implementation of game API
type AppStack t = SDLT t (SyncT t (TimerT t (NetworkT t (LoggingT t (GameMonad t)))))

-- | Wrapper around 'AppStack'
newtype AppMonad t a = AppMonad { runAppMonad :: AppStack t a}
  deriving (Functor, Applicative, Monad, MonadFix)

deriving instance (ReflexHost t, MonadCatch (HostFrame t)) => MonadCatch (AppMonad t)
deriving instance (ReflexHost t, MonadThrow (HostFrame t)) => MonadThrow (AppMonad t)
deriving instance (ReflexHost t, MonadMask (HostFrame t)) => MonadMask (AppMonad t)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => MonadIO (AppMonad t)
deriving instance (ReflexHost t, MonadIO (HostFrame t), MonadCatch (HostFrame t)) => MonadSDL t (AppMonad t)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => LoggingMonad t (AppMonad t)
deriving instance (ReflexHost t) => MonadSample t (AppMonad t)
deriving instance (ReflexHost t) => MonadHold t (AppMonad t)
deriving instance (ReflexHost t) => MonadSubscribeEvent t (AppMonad t)
deriving instance (ReflexHost t, MonadCatch (HostFrame t), MonadBaseControl IO (HostFrame t), MonadIO (HostFrame t)) => NetworkMonad t (AppMonad t)
deriving instance (ReflexHost t, MonadCatch (HostFrame t), MonadBaseControl IO (HostFrame t), MonadIO (HostFrame t)) => NetworkClient t (AppMonad t)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => TimerMonad t (AppMonad t)
deriving instance (ReflexHost t, MonadMask (HostFrame t), MonadIO (HostFrame t)) => SyncMonad t (AppMonad t)

instance ReflexHost t => MonadReflexCreateTrigger t (AppMonad t) where
  newEventWithTrigger = AppMonad . newEventWithTrigger
  newFanEventWithTrigger trigger = AppMonad $ newFanEventWithTrigger trigger

instance (ReflexHost t, MonadIO (HostFrame t), MonadMask (HostFrame t), MonadBaseControl IO (HostFrame t)) => GameModule t (AppMonad t) where
  type ModuleOptions t (AppMonad t) = SyncOptions (NetworkOptions ())
  runModule opts m = runModule opts $ runAppMonad m
  withModule t _ = withModule t (Proxy :: Proxy (AppStack t))

instance (ReflexHost t, MonadIO (HostFrame t)) => MonadAppHost t (AppMonad t) where
  getFireAsync = AppMonad getFireAsync
  getRunAppHost = do
    runner <- AppMonad getRunAppHost
    return $ \m -> runner $ runAppMonad m
  performPostBuild_ = AppMonad . performPostBuild_
  liftHostFrame = AppMonad . liftHostFrame

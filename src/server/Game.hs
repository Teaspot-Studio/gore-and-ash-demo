module Game(
    game
  , AppMonad
  ) where

import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Sync
import Game.GoreAndAsh.Time

-- | Application monad that is used for implementation of game API
type AppMonad = SyncT Spider (TimerT Spider (NetworkT Spider (LoggingT Spider(GameMonad Spider))))

-- | Server logic
game :: AppMonad ()
game = return ()
module Game(
    playGame
  , AppMonad
  ) where

import Game.Monad

-- | Client logic
playGame :: AppFrame t => AppMonad t ()
playGame = return ()
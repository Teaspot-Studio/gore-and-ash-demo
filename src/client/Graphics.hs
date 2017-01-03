module Graphics(
    drawFrame
  ) where

import Game.GoreAndAsh
import Game.GoreAndAsh.SDL

import Data.Map.Strict (Map)
import Game
import Game.Camera
import Game.Client.Player
import Game.Monad
import Game.Player
import Graphics.Square

-- | Here all code needed to draw a single game frame is located
drawFrame :: forall t . AppFrame t => Dynamic t Game
  -> Window -- ^ Window where to draw
  -> Renderer  -- ^ Renderer to use
  -> HostFrame t ()
drawFrame gameDyn w r = do
  rendererDrawColor r $= V4 200 200 200 255
  clear r
  Game{..} <- sample . current $ gameDyn
  drawPlayers w r gamePlayers gameCamera

-- | Draw players
drawPlayers :: forall t . AppFrame t
  => Window -- ^ Window where to draw
  -> Renderer -- ^ Renderer to use
  -> Map PlayerId ClientPlayer -- ^ Players collection
  -> Camera -- ^ User camera (defines transformation of canvas)
  -> HostFrame t ()
drawPlayers w r playersMap cam = mapM_ drawPlayer playersMap
  where
    drawPlayer Player{..} = do
      renderSquare w r cam playerSize playerPos playerColor

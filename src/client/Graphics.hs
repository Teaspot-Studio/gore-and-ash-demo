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
  drawPlayer w r gameCamera gameLocalPlayer
  drawPlayers w r gameCamera gamePlayers

-- | Draw players
drawPlayers :: forall t . AppFrame t
  => Window -- ^ Window where to draw
  -> Renderer -- ^ Renderer to use
  -> Camera -- ^ User camera (defines transformation of canvas)
  -> Map PlayerId ClientPlayer -- ^ Players collection
  -> HostFrame t ()
drawPlayers w r cam = mapM_ (drawPlayer w r cam)

-- | Draw single player
drawPlayer :: forall t . AppFrame t
  => Window -- ^ Window where to draw
  -> Renderer -- ^ Renderer to use
  -> Camera -- ^ User camera (defines transformation of canvas)
  -> ClientPlayer -- ^ Player to render
  -> HostFrame t ()
drawPlayer w r cam Player{..} = do
  renderSquare w r cam playerSize playerPos playerColor

module Graphics(
    drawFrame
  ) where

import Control.Monad
import Data.Map.Strict (Map)
import Game.GoreAndAsh
import Game.GoreAndAsh.SDL
import SDL.TTF.FFI (TTFFont)
import Control.Monad.IO.Class

import Game
import Game.Camera
import Game.Client.Player
import Game.Global
import Game.Monad
import Game.Player
import Graphics.Square

import qualified Data.Map.Strict as M
import qualified SDL.Raw as SDL
import qualified SDL.TTF as TTF

-- | Here all code needed to draw a single game frame is located
drawFrame :: forall t . AppFrame t => Dynamic t Game
  -> TTFFont -- ^ Font to render text with
  -> Window -- ^ Window where to draw
  -> Renderer  -- ^ Renderer to use
  -> HostFrame t ()
drawFrame gameDyn font w r = do
  rendererDrawColor r $= V4 200 200 200 255
  clear r
  Game{..} <- sample . current $ gameDyn
  drawPlayer w r gameCamera gameLocalPlayer
  drawPlayers w r gameCamera gamePlayers
  drawScore w r font gamePlayers
  --updateWindowSurface w

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

-- | Draw global info about game (score)
drawScore :: forall t . AppFrame t
  => Window -- ^ Window where to draw
  -> Renderer -- ^ Renderer to use
  -> TTFFont -- ^ Font to use to render text with
  -> Map PlayerId ClientPlayer -- ^ Players collection
  -> HostFrame t ()
drawScore w _ font players = unless (null players) $ do
  winSurf <- getWindowSurface w
  surf <- TTF.renderUTF8Solid font msg $ SDL.Color 0 0 0 0
  surfaceBlit surf Nothing winSurf Nothing
  freeSurface surf
  where
    msg = unlines . M.elems . M.mapWithKey mkLine $ playerScore <$> players
    mkLine i n = "Player " ++ show (unPlayerId i) ++ ": " ++ show n


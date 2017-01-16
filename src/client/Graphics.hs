module Graphics(
    drawFrame
  ) where

import Control.Monad
import Data.Map.Strict (Map)
import Game.GoreAndAsh
import Game.GoreAndAsh.SDL
import SDL.TTF.FFI (TTFFont)

import Game
import Game.Bullet
import Game.Camera
import Game.Client.Player
import Game.Monad
import Game.Player
import Graphics.Bullet
import Graphics.Square

import qualified Data.Foldable as F
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
  mapM_ (drawPlayer w r gameCamera) gamePlayers
  mapM_ (drawBullet w r gameCamera) gameBullets
  let allPlayers = M.insert
        (playerCustom gameLocalPlayer)
        (fmap (const ()) gameLocalPlayer)
        gamePlayers
  drawScore w r font allPlayers
  glSwapWindow w

-- | Draw single player
drawPlayer :: forall t s . AppFrame t
  => Window -- ^ Window where to draw
  -> Renderer -- ^ Renderer to use
  -> Camera -- ^ User camera (defines transformation of canvas)
  -> Player s -- ^ Player to render
  -> HostFrame t ()
drawPlayer w r cam Player{..} = do
  renderSquare w r cam playerSize playerPos playerColor

-- | Draw single bullet
drawBullet :: forall t s . AppFrame t
  => Window -- ^ Window where to draw
  -> Renderer -- ^ Renderer to use
  -> Camera -- ^ User camera (defines transformation of canvas)
  -> Bullet s -- ^ Bullet to render
  -> HostFrame t ()
drawBullet w r cam Bullet{..} = do
  renderBullet w r bulletPos bulletVel cam

-- | Draw global info about game (score)
drawScore :: forall t . AppFrame t
  => Window -- ^ Window where to draw
  -> Renderer -- ^ Renderer to use
  -> TTFFont -- ^ Font to use to render text with
  -> Map PlayerId ClientPlayer -- ^ Players collection
  -> HostFrame t ()
drawScore _ r font players = unless (null players) $ do
  _ <- F.foldlM drawTextLine 5 msgs
  return ()
  where
    msgs = M.elems . M.mapWithKey mkLine $ playerScore <$> players
    mkLine i n = "Player " ++ show (unPlayerId i) ++ ": " ++ show n

    color = SDL.Color 0 0 0 0
    xoffset = 20
    yoffset = 50

    drawTextLine y msg = do
      surf <- TTF.renderUTF8Solid font msg color
      size <- surfaceDimensions surf
      tex <- createTextureFromSurface r surf
      copy r tex Nothing (Just $ Rectangle (P $ V2 xoffset y) size)
      destroyTexture tex
      freeSurface surf
      return $ y + yoffset


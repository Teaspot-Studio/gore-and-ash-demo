{-# LANGUAGE OverloadedLists #-}
module Graphics(
    drawFrame
  ) where

import Control.Monad
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text (Text, pack)
import Game.GoreAndAsh.SDL
import SDL.Font (Font)
import SDL.Vect (V4(..))

import Game
import Game.Bullet
import Game.Camera
import Game.Client.Player
import Game.Player
import Graphics.Bullet
import Graphics.Square

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified SDL.Font as Font

showt :: Show a => a -> Text
showt = pack . show

-- | Here all code needed to draw a single game frame is located
drawFrame :: Game
  -> Font -- ^ Font to render text with
  -> Window -- ^ Window where to draw
  -> Maybe Renderer  -- ^ Renderer to use
  -> IO ()
drawFrame _ _ _ Nothing = putStrLn "Invalid renderer!"
drawFrame Game{..} font w (Just r) = do
  rendererDrawColor r $= V4 200 200 200 255
  clear r
  drawPlayer w r gameCamera gameLocalPlayer
  mapM_ (drawPlayer w r gameCamera) gamePlayers
  mapM_ (drawBullet w r gameCamera) gameBullets
  let allPlayers = M.insert
        (playerCustom gameLocalPlayer)
        (fmap (const ()) gameLocalPlayer)
        gamePlayers
  drawScore w r font allPlayers
  present r

-- | Draw single player
drawPlayer :: Window -- ^ Window where to draw
  -> Renderer -- ^ Renderer to use
  -> Camera -- ^ User camera (defines transformation of canvas)
  -> Player s -- ^ Player to render
  -> IO ()
drawPlayer w r cam Player{..} = do
  renderSquare w r cam playerSize playerPos playerColor

-- | Draw single bullet
drawBullet :: Window -- ^ Window where to draw
  -> Renderer -- ^ Renderer to use
  -> Camera -- ^ User camera (defines transformation of canvas)
  -> Bullet s -- ^ Bullet to render
  -> IO ()
drawBullet w r cam Bullet{..} = do
  renderBullet w r bulletPos bulletVel cam

-- | Draw global info about game (score)
drawScore :: Window -- ^ Window where to draw
  -> Renderer -- ^ Renderer to use
  -> Font -- ^ Font to use to render text with
  -> Map PlayerId ClientPlayer -- ^ Players collection
  -> IO ()
drawScore _ r font players = unless (null players) $ do
  _ <- F.foldlM drawTextLine 5 msgs
  return ()
  where
    msgs = M.elems . M.mapWithKey mkLine $ playerScore <$> players
    mkLine i n = "Player " <> showt (unPlayerId i) <> ": " <> showt n

    color = V4 0 0 0 0
    xoffset = 20
    yoffset = 50

    drawTextLine y msg = do
      surf <- Font.solid font color msg
      size <- surfaceDimensions surf
      tex <- createTextureFromSurface r surf
      copy r tex Nothing (Just $ Rectangle (P $ V2 xoffset y) size)
      destroyTexture tex
      freeSurface surf
      return $ y + yoffset

module Graphics.Bullet(
    renderBullet
  ) where

import Control.Monad.IO.Class
import Game.Camera
import Game.GoreAndAsh
import Game.GoreAndAsh.SDL
import SDL

-- | Function of rendering player
renderBullet :: MonadIO m => Window -> Renderer -> V2 Double -> V2 Double -> Camera -> m ()
renderBullet window renderer pos vel c = do
  wsize <- fmap (fmap fromIntegral) . get $ windowSize window
  rendererDrawColor renderer $= V4 0 0 0 255
  drawLine renderer (applyTrans wsize startPoint) (applyTrans wsize endPoint)
  where
    velScaleFactor = 0.04
    startPoint = negate (velScaleFactor * 0.5 * vel)
    endPoint = velScaleFactor * 0.5 * vel

    applyTrans wsize = P . fmap round . applyTransform2D (modelMtx wsize)

    modelMtx :: V2 Double -> M33 Double
    modelMtx wsize = viewportTransform2D 0 wsize !*! cameraMatrix c !*! translate2D pos
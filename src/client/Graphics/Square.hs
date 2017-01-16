module Graphics.Square(
    renderSquare
  ) where

import Control.Monad.IO.Class
import Data.Word
import Foreign.C.Types
import Game.Camera
import Game.GoreAndAsh
import Game.GoreAndAsh.SDL
import SDL

-- | Function of rendering player
renderSquare :: MonadIO m => Window -> Renderer -> Camera -> Double -> V2 Double -> V3 Double -> m ()
renderSquare window renderer c size pos col  = do
  wsize <- fmap (fmap fromIntegral) . get $ windowSize window
  rendererDrawColor renderer $= transColor col
  fillRect renderer $ Just $ transformedSquare wsize
  where
    transColor :: V3 Double -> V4 Word8
    transColor (V3 r g b) = V4 (round $ r * 255) (round $ g * 255) (round $ b * 255) 255

    modelMtx :: V2 Double -> M33 Double
    modelMtx wsize = viewportTransform2D 0 wsize !*! cameraMatrix c !*! translate2D pos

    transformedSquare :: V2 Double -> Rectangle CInt
    transformedSquare wsize = Rectangle (P topleft) (botright - topleft)
      where
      topleft = fmap round . applyTransform2D (modelMtx wsize) $ V2 (-size/2) (-size/2)
      botright = fmap round . applyTransform2D (modelMtx wsize) $ V2 (size/2) (size/2)
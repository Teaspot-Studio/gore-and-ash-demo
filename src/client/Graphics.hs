module Graphics(
    drawFrame
  ) where

import Control.Monad.IO.Class
import Foreign.C
import SDL (get)

import Game.GoreAndAsh
import Game.GoreAndAsh.SDL

drawFrame :: forall t . (ReflexHost t, MonadIO (HostFrame t))
  => Window -> Renderer -> HostFrame t ()
drawFrame _ r = do
  rendererDrawColor r $= V4 0 0 0 0
  clear r
  rendererDrawColor r $= V4 250 0 0 0
  ws <- getCurrentSize
  let squareRect :: Rectangle Double
      squareRect = Rectangle (P $ V2 0.1 0.1) (V2 0.8 0.8)
  fillRect r (Just $ resizeRect ws squareRect)
  where
    getCurrentSize :: HostFrame t (V2 CInt)
    getCurrentSize = do
      vp <- get (rendererViewport r)
      case vp of
        Nothing -> return 0
        Just (Rectangle _ s) -> return s

    resizeRect :: V2 CInt -> Rectangle Double -> Rectangle CInt
    resizeRect (V2 vw vh) (Rectangle (P (V2 x y)) (V2 w h)) = Rectangle (P (V2 x' y')) (V2 w' h')
      where
        x' = round $ x * fromIntegral vw
        y' = round $ y * fromIntegral vh
        w' = round $ w * fromIntegral vw
        h' = round $ h * fromIntegral vh

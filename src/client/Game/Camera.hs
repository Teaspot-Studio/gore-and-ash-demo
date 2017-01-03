module Game.Camera(
    Camera(..)
  , camera
  , cameraMatrix
  , cameraToWorld
  , cameraFromWorld
  ) where

import Control.Lens
import GHC.Generics (Generic)
import Linear

import Game.Monad
import Game.GoreAndAsh
import Game.GoreAndAsh.SDL

data Camera = Camera {
  cameraPos  :: !(V2 Double)
, cameraZoom :: !Double
} deriving (Generic)

camera :: forall t . AppFrame t => WindowWidget t -> AppMonad t (Dynamic t Camera)
camera w = do
        moveCamera (V2 0              (-cameraSpeed)) ScancodeS
    =<< moveCamera (V2 0              cameraSpeed   ) ScancodeW
    =<< moveCamera (V2 cameraSpeed    0             ) ScancodeD
    =<< moveCamera (V2 (-cameraSpeed) 0             ) ScancodeA
    =<< zoomCamera 0.1 initialCamera
  where
    cameraSpeed :: Double
    cameraSpeed = 0.1

    initialCamera = Camera {
        cameraPos  = 0
      , cameraZoom = 1
      }

    moveCamera :: V2 Double -> Scancode -> Dynamic t Camera -> AppMonad t (Dynamic t Camera)
    moveCamera dv k cDyn = do
      pressDyn <- keyPressing w k
      return $ do
        c <- cDyn
        mpress <- pressDyn
        return $ case mpress of
          Nothing -> c
          Just _  -> c {
              cameraPos = cameraPos c + dv
            }

    zoomCamera :: Double -> Camera -> AppMonad t (Dynamic t Camera)
    zoomCamera z c0 = do
      let scrollE = fmap fromIntegral $ mouseScrollY w
          mkNewCam k c = c {
            cameraZoom = min 3 $ max 0.01 $ cameraZoom c + 0.2 * z * k
          }
      foldDyn mkNewCam c0 scrollE

-- | Calculate transformation matrix for camera
cameraMatrix :: Camera -> M33 Double
cameraMatrix Camera{..} = translate2D (V2 (-cameraPos^._x) (-cameraPos^._y))
  !*! scale2D (V2 (-cameraZoom) (-cameraZoom))

-- | Transform camera local coords to world
cameraToWorld :: Camera -> V2 Double -> V2 Double
cameraToWorld c v = inv33 (cameraMatrix c) `applyTransform2D` v

-- | Transform world coords to camera coords
cameraFromWorld :: Camera -> V2 Double -> V2 Double
cameraFromWorld c v = cameraMatrix c `applyTransform2D` v
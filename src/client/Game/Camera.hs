module Game.Camera(
    Camera(..)
  , camera
  , cameraMatrix
  , cameraToWorld
  , cameraFromWorld
  ) where

import Control.Lens
import Data.Monoid
import GHC.Generics (Generic)
import Linear

import qualified Data.Foldable as F

import Game.Monad
import Game.GoreAndAsh
import Game.GoreAndAsh.SDL
import Game.GoreAndAsh.Time

data Camera = Camera {
  cameraPos  :: !(V2 Double)
, cameraZoom :: !Double
} deriving (Generic)

-- | Available actions with camera
data CameraAction = MoveCamera (V2 Double) | ZoomCamera Double

-- | Update camera according given action
applyCamAction :: Camera -> CameraAction -> Camera
applyCamAction c a = case a of
  MoveCamera dv -> c { cameraPos = cameraPos c + dv }
  ZoomCamera k -> c {
      cameraZoom = min 3 $ max 0.01 $ cameraZoom c + k
    }

camera :: forall t . AppFrame t => WindowWidget t -> AppMonad t (Dynamic t Camera)
camera w = do
  let dt = 0.01 :: Double
  tickE <- fmap (const dt) <$> tickEvery (realToFrac dt)
  let
    dvDown  = V2 0              (-cameraSpeed)
    dvUp    = V2 0              cameraSpeed
    dvLeft  = V2 (-cameraSpeed) 0
    dvRight = V2 cameraSpeed    0
  downE  <- fmap (MoveCamera . (dvDown  *)) <$> pressingEvent tickE ScancodeS
  upE    <- fmap (MoveCamera . (dvUp    *)) <$> pressingEvent tickE ScancodeW
  leftE  <- fmap (MoveCamera . (dvLeft  *)) <$> pressingEvent tickE ScancodeA
  rightE <- fmap (MoveCamera . (dvRight *)) <$> pressingEvent tickE ScancodeD
  let
    zoomE = ZoomCamera . (* 0.05) . fromIntegral <$> mouseScrollY w

    updE :: Event t [CameraAction]
    updE = fmap pure downE
      <> fmap pure upE
      <> fmap pure leftE
      <> fmap pure rightE
      <> fmap pure zoomE

    updCamera :: [CameraAction] -> Camera -> Camera
    updCamera as c = F.foldl' applyCamAction c as

  foldDyn updCamera initialCamera updE
  where
    cameraSpeed :: Double
    cameraSpeed = 0.4

    initialCamera = Camera {
        cameraPos  = 0
      , cameraZoom = 0.01
      }

    -- While key pressed generate the following event
    pressingEvent :: Event t Double -> Scancode -> AppMonad t (Event t (V2 Double))
    pressingEvent e scode = do
      pressDyn <- keyPressing w scode
      let mkTag mpress v = const (V2 v v) <$> mpress
      return $ attachPromptlyDynWithMaybe mkTag pressDyn e

-- | Calculate transformation matrix for camera
cameraMatrix :: Camera -> M33 Double
cameraMatrix Camera{..} = translate2D (V2 (-cameraPos^._x) (-cameraPos^._y))
  !*! scale2D (V2 cameraZoom cameraZoom)

-- | Transform camera local coords to world
cameraToWorld :: Camera -> V2 Double -> V2 Double
cameraToWorld c v = inv33 (cameraMatrix c) `applyTransform2D` v

-- | Transform world coords to camera coords
cameraFromWorld :: Camera -> V2 Double -> V2 Double
cameraFromWorld c v = cameraMatrix c `applyTransform2D` v
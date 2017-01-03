{-# OPTIONS_GHC -fno-warn-orphans #-}
module Store() where

import Data.Store
import Linear

instance Store a => Store (V1 a)
instance Store a => Store (V2 a)
instance Store a => Store (V3 a)
instance Store a => Store (V4 a)

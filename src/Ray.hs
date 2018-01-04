module Ray where

import Vector

data Ray = Ray Vector Vector deriving Show

at:: Ray -> Double -> Vector
at (Ray startVector direction) t =
  vectorSum startVector $ mul t direction


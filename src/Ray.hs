module Ray where

import Vector

data Ray = Ray Vector UnitVector deriving Show

at:: Ray -> Double -> Vector
at (Ray startVector direction) t =
  vecSum startVector $ vecMul t direction


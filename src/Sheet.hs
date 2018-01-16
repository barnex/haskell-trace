module Sheet where

import Ray
import Shape
import Vector

sheet::Double -> UnitVector -> Shape
sheet distanceFromOrigin normalVector =
  \env ray@(Ray start dir) ->
    let rs = start `dot` normalVector in
    let rd = dir `dot` normalVector in
    let dist = (distanceFromOrigin - rs) / rd in
    if dist > 0 then
      Just (dist, normalVector)
    else
      Nothing
    

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
    

-- | sheety constructs a horizontal sheet
sheety::Double -> Shape
sheety height =
  \env ray@(Ray start dir) ->
    let (_,sy,_) = elements start in
    let (_,dy,_) = elements dir in
    let t = (height - sy) / dy in
    if t > 0 then
      Just (t, normalize (vector 0.0 1.0 0.0))
    else
      Nothing

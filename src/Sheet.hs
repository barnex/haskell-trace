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
    let (Vector _ sy _) = start in
    let (Vector _ dy _) = dir in
    let t = (height - sy) / dy in
    if t > 0 then
      Just (t, (Vector 0.0 1.0 0.0))
    else
      Nothing

module Sphere where

import Ray
import Shape
import Vector

type Center = Vector
type Radius = Double

sphere::Center -> Radius -> Shape
sphere center r =
-- type Shape = Env -> Ray -> Maybe (Distance, UnitVector)
  \env ray@(Ray start dir) -> 
    let v = vecSub start center in
    let t = dot v dir in
    let discr = (t*t) - len2 v +  (r*r) in
    let dist = (-(dot v dir)) - sqrt discr in
    let point = at ray dist in
    let normal = normalize $ vecSub point center in
    if discr < 0 then Nothing else 
      if dist < 0 then Nothing else
        Just (dist, normal)

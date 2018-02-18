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
    let discr = (dot v dir ^^ 2) - len2 v +  r ^^ 2 in
    let dist = (-(dot v dir)) - sqrt discr in
    let point = at ray dist in
    let normal = normalize $ vecSub point center in
    if discr < 0 then Nothing else Just (dist, normal)

module Sphere where

import Ray
import Shape
import Vector

type Center = Vector
type Radius = Double

sphere::Center -> Radius -> Shape
sphere center r =
-- type Shape = Env -> Ray -> Maybe (Distance, UnitVector)
  \env (Ray start dir) -> 
    let v = sub start center in
    let discr = (dot v dir ^^ 2) - len2 v +  r ^^ 2 in
    let dist = (-(dot v dir)) - sqrt discr in
    if discr < 0 then Nothing else Just (dist, normal)

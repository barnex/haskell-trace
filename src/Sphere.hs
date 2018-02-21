module Sphere where

import Ray
import Shape
import Vector

import Data.List

type Center = Vector
type Radius = Double

sphere::Center -> Radius -> Shape
sphere center r =
-- type Shape = Env -> Ray -> Maybe (Distance, UnitVector)
  \env ray@(Ray start dir) -> 
    let v = vecSub start center
        t = dot v dir 
        discr = (t*t) - len2 v +  (r*r)
    in
    if discr < 0 then Nothing else 
      let d1 = -t - sqrt discr
          d2 = -t + sqrt discr
      in
      case (sort $ filter (\x -> x >= 0) [d1, d2]) of
        [] -> Nothing
        d:_ -> let point = ray `at` d
                   normal = normalize $ vecSub point center
               in
               Just (d, normal)
module Sphere where

import Ray
import Shape
import Vector

import Data.List

type Center = Vector
type Radius = Double

vsphere::Center -> Radius -> Shape
vsphere center r =
-- type Shape = Env -> Ray -> Maybe (Distance, UnitVector)
  \env ray@(Ray start dir) -> 
    let v = vecSub start center in
    let t = dot v dir in
    let discr = (t*t) - len2 v +  (r*r) in
    let dist = (-(dot v dir)) - sqrt discr in
    let point = ray `at` dist in
    let normal = normalize $ vecSub point center in
    if discr < 0 then Nothing else 
      if dist < 0 then Nothing else
        Just (dist, normal)

sphere::Center -> Radius -> Shape
sphere center r =
-- type Shape = Env -> Ray -> Maybe (Distance, UnitVector)
  \env ray@(Ray start dir) -> 
    let a = dot dir dir
        b = 2.0 * (dot (start `vecSub` center) dir)
        c = len2 (start `vecSub` center) - (r*r)
        discr = b*b - 4*a*c
    in
    if discr < 0.0 then Nothing else
      let d1 = (- b + sqrt discr) / (2.0*a)
          d2 = (- b - sqrt discr) / (2.0*a)
      in
      case (sort $ filter (\x -> x >= 0.0) [d1, d2]) of
        [] -> Nothing
        d:_ -> let point = ray `at` d
                   normal = normalize $ vecSub point center
                in
                Just (d, normal)
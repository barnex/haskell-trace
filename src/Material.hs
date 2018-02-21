module Material where

import Data.List
import Data.Maybe
import Object
import Ray
import Shape
import Types
import Vector

type Material = Env -> Ray -> RecDepth -> Distance -> UnitVector -> Colour

paint::Shape -> Material -> Object
paint shape material = \env ray recDepth -> do 
  (dist, normal) <- shape env ray
  let colour = material env ray recDepth dist normal
  return (dist, colour)

flat::Colour -> Material
flat colour = \_ _ _ _ _ -> colour

clamp::Double -> Double
clamp d = max d 0

diffuse::Colour -> Material
diffuse colour = \env ray _ dist normal ->
  let p = pointOnShape ray dist normal 
      v = vectorTowardsLight (light env) p
      n = normalize v 
      rayTowardsLight = Ray p n 
  in
  if occludes env rayTowardsLight v then
    Colour 0.0 0.0 0.0
  else
    let fallOff = 1/len2 v in
    let cosTheta = (n `dot` normal) * fallOff in
    clamp cosTheta `scale` colour

pointOnShape :: Ray -> Double -> UnitVector -> Vector
pointOnShape ray dist normal =
  (ray `at` dist) `vecSum` (vecMul 1e-6 normal)

vectorTowardsLight :: Vector -> Vector -> Vector
vectorTowardsLight light pointOnShape =
  light `vecSub` pointOnShape

specular :: Double -> Material
specular specularPower = \env ray@(Ray _ dir) _ dist normal ->
  let reflectedDir = reflected dir normal
      p = pointOnShape ray dist normal
      v = vectorTowardsLight (light env) p
      n = normalize v 
      rayTowardsLight = Ray p n 
      specularComponent = (reflectedDir `dot` n) ** specularPower
  in
  if occludes env rayTowardsLight v then
      Colour 0.0 0.0 0.0
  else
    clamp specularComponent `scale` (Colour 1.0 1.0 1.0) -- TODO: this should be the colour of the light

-- TODO: Koen: reflectivity type
reflective::Colour -> Material
reflective reflectivity = \env ray@(Ray _ dir) recDepth dist normal ->
  let pointOnShape = ray `at` dist in
 -- let dir' = normalize $ vecMul (1.0) dir in -- TODO: why is this 1.0 and not -1.0 ????
  let reflectedDir = reflected dir normal in
  let reflectedRay = offset 1e-6 $ Ray pointOnShape reflectedDir in
  findColour env reflectedRay (recDepth - 1)

reflected:: UnitVector -> UnitVector -> UnitVector
reflected dir normal = normalize $ dir `vecSub` (vecMul (2.0*(normal `dot` dir)) normal)
  
occludes::Env -> Ray -> Vector -> Bool
occludes env ray vectorTowardsLight =
  let s = scene env in
    let f = \o -> o env ray (-1) in
    let maybeDistancesAndColours = fmap f s in
    let sortedTs = sortOn fst $ catMaybes maybeDistancesAndColours in
-- TODO(Kasper): factor out
     case sortedTs of
       [] -> False
       ((t,_):_) -> t >= 0.0 && t < len vectorTowardsLight

flatWhite:: Material
flatWhite = flat (Colour 1.0 1.0 1.0)

combine :: Material -> Material -> Material
combine m1 m2 =
  \env ray recDepth dist normal ->
    let c1 = m1 env ray recDepth dist normal
        c2 = m2 env ray recDepth dist normal
    in
    blend c1  c2
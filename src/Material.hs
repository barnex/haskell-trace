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
  let pointOnShape = ray `at` dist in
  let vectorTowardsLight = (light env) `vecSub` pointOnShape in
  let normalizedVectorTowardsLight = normalize vectorTowardsLight in
  let rayTowardsLight = Ray pointOnShape normalizedVectorTowardsLight in
  if occludes env rayTowardsLight vectorTowardsLight then
    Colour 0.0 0.0 0.0
  else
    let fallOff = 1/len2 vectorTowardsLight in
    let cosTheta = (normalizedVectorTowardsLight `dot` normal) * fallOff in
    clamp cosTheta `scale` colour


-- TODO: Koen: reflectivity type
reflective::Colour -> Material
reflective reflectivity = \env ray@(Ray _ dir) recDepth dist normal ->
  let pointOnShape = ray `at` dist in
  let dir' = normalize $ vecMul (-1.0) dir in -- direction pointing outward from surface
  let reflectedDir = confusingFormula dir' normal in
  --let reflectedDir = dir' `sub` (mul (2.0*(normal `dot` dir')) normal) in
  let reflectedRay = offset 1e-9 $ Ray pointOnShape reflectedDir in
  findColour env reflectedRay (recDepth - 1)
  
confusingFormula:: UnitVector -> UnitVector -> UnitVector
confusingFormula dir' normal = normalize $ dir' `vecSub` (vecMul (2.0*(normal `dot` dir')) normal)
  
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

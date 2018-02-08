
module Material where

import Data.List
import Data.Maybe
import Object
import Ray
import Shape
import Types
import Vector

type Material = Env -> Ray -> Distance -> UnitVector -> Colour

paint::Shape -> Material -> Object
paint shape material = \env ray -> do 
  (dist, normal) <- shape env ray
  let colour = material env ray dist normal
  return (dist, colour)

flat::Colour -> Material
flat colour = \env ray dist norm -> colour

clamp::Double -> Double
clamp d = max d 0

diffuse::Colour -> Material
diffuse colour = \env ray dist normal ->
  let pointOnShape = ray `at` dist in
  let vectorTowardsLight = (light env) `sub` pointOnShape in
  let normalizedVectorTowardsLight = normalize vectorTowardsLight in
  let rayTowardsLight = Ray pointOnShape normalizedVectorTowardsLight in
  if occludes env rayTowardsLight vectorTowardsLight then
    Colour 0.0 0.0 0.0
  else
    let fallOff = 1/len2 vectorTowardsLight in
    let cosTheta = (normalizedVectorTowardsLight `dot` normal) * fallOff in
    clamp cosTheta `scale` colour
  
occludes::Env -> Ray -> Vector -> Bool
occludes env ray vectorTowardsLight =
  let s = scene env in
    let f = \o -> o env ray in
    let maybeDistancesAndColours = fmap f s in
    let sortedTs = sortOn fst $ catMaybes maybeDistancesAndColours in
-- TODO(Kasper): factor out
     case sortedTs of
       [] -> False
       ((t,_):xs) -> t > 0 && t < len vectorTowardsLight

flatWhite::Material
flatWhite = flat $ Colour 1.0 1.0 1.0

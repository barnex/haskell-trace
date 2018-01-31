
module Material where

import Vector
import Ray
import Types
import Shape
import Object

light :: Vector
light = Vector 1.0 1.0 0.0

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
  let vectorTowardsLight = light `sub` pointOnShape in
  let normalizedVectorTowardsLight = normalize vectorTowardsLight in
  let fallOff = 1/len2 vectorTowardsLight in
  let cosTheta = (normalizedVectorTowardsLight `dot` normal) * fallOff in
  clamp cosTheta `scale` colour
  

flatWhite::Material
flatWhite = flat $ Colour 1.0 1.0 1.0

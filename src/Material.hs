module Material where

import Data.List
import Data.Maybe
import Object
import Ray
import Shape
import Types
import Vector as Vector
import System.Random

type Material = Env -> Ray -> RecDepth -> Distance -> UnitVector -> JefDeMonad Colour

paint::Shape -> Material -> Object
paint shape material = \env ray recDepth -> do 
  let maybeDistNormal = shape env ray
  case maybeDistNormal of
    Just (dist, normal) -> do
      colour <- material env ray recDepth dist normal
      return $ Just (dist, colour)
    Nothing -> return Nothing

flat::Colour -> Material
flat colour = \_ _ _ _ _ -> return colour

clamp::Double -> Double
clamp d = max d 0

makeSureNormalPointsOutwards :: Ray -> UnitVector -> UnitVector
makeSureNormalPointsOutwards (Ray _ dir) normal =
  if (dir `dot` normal) > 0 then
    normalize $ vecMul (-1.0) normal
  else
    normal

indirectLight::Env -> UnitVector -> Vector -> (Colour, Env)
indirectLight env norm position =
--  let randomGen = randomGenerator env in
--      (randomX,  = 
  undefined

generateRandomVector::StdGen -> UnitVector -> UnitVector
generateRandomVector randomGen normal =
  let (randomX, randomGen') = randomR (-1.0, 1.0) randomGen in
  let (randomY, randomGen'') = randomR (-1.0, 1.0)  randomGen' in
  let (randomZ, randomGen''') = randomR (-1.0, 1.0)  randomGen'' in
  let vect = Vector.vector randomX randomY randomZ in
  let squareLen = len2 vect in
  if squareLen > 1 then
    generateRandomVector randomGen''' normal
  else
    let normalized = normalize vect in
    if normalized `dot` normal > 0 then
      normalized
    else
      normalize $ (-1.0) `vecMul` normalized 


diffuse::Colour -> Material
diffuse colour = \env ray recDepth dist normal' -> do
  let normal = makeSureNormalPointsOutwards ray normal'
  let p = pointOnShape ray dist normal 
  let randDirection = generateRandomVector (undefined env) normal
-- Kasper extract this in a function, the indirects tuff
  let indirectRay = Ray p randDirection
  indirectColour <- findColour env indirectRay $ recDepth -1 
  let indirectTheta =  normal `dot` randDirection
  let scaledColour = indirectTheta `scale` indirectColour
  let v = vectorTowardsLight (light env) p
  let n = normalize v 
  let rayTowardsLight = Ray p n 
  doesOcclude <- occludes env rayTowardsLight v
  if doesOcclude then
-- @Arne : something with several PIs
    return $ Colour 0.0 0.0 0.0 `blend` scaledColour
  else
    do
      let fallOff = 1/len2 v
      let cosTheta = (n `dot` normal) * fallOff
      let diffuseColour = clamp cosTheta `scale` colour
      return $ diffuseColour `blend` scaledColour

pointOnShape :: Ray -> Double -> UnitVector -> Vector
pointOnShape ray dist normal =
  (ray `at` dist) `vecSum` (vecMul 1e-4 normal)

vectorTowardsLight :: Vector -> Vector -> Vector
vectorTowardsLight light pointOnShape =
  light `vecSub` pointOnShape

specular :: Double -> Material
specular specularPower = \env ray@(Ray _ dir) _ dist normal' ->
  let normal = makeSureNormalPointsOutwards ray normal' 
      reflectedDir = reflected dir normal
      p = pointOnShape ray dist normal
      v = vectorTowardsLight (light env) p
      n = normalize v 
      rayTowardsLight = Ray p n 
      specularComponent = (reflectedDir `dot` n) ** specularPower
  in 
    do
  doesOcclude <- occludes env rayTowardsLight v
  if doesOcclude then
      return $ Colour 0.0 0.0 0.0
  else
    -- TODO: add falloff
    return $ clamp specularComponent `scale` (Colour 1.0 1.0 1.0) -- TODO: this should be the colour of the light

-- TODO: Koen: reflectivity type
reflective::Colour -> Material
reflective reflectivity = \env ray@(Ray _ dir) recDepth dist normal' ->
  let normal = makeSureNormalPointsOutwards ray normal' in 
  let pointOnShape = ray `at` dist in
 -- let dir' = normalize $ vecMul (1.0) dir in -- TODO: why is this 1.0 and not -1.0 ????
  let reflectedDir = reflected dir normal in
  let reflectedRay = offset 1e-6 $ Ray pointOnShape reflectedDir in
    do
      materialColour <- findColour env reflectedRay (recDepth - 1)
      return $ reflectivity `mulColour` materialColour

reflected:: UnitVector -> UnitVector -> UnitVector
reflected dir normal = normalize $ dir `vecSub` (vecMul (2.0*(normal `dot` dir)) normal)
  
occludes::Env -> Ray -> Vector -> JefDeMonad Bool
occludes env ray vectorTowardsLight = do
  let s = scene env
  let f = \o -> o env ray (-1)
  maybeDistancesAndColours <- sequence $ f <$> s
  let sortedTs = sortOn fst $ catMaybes maybeDistancesAndColours
-- TODO(Kasper): factor out
  case sortedTs of
    [] -> return $ False
    ((t,_):_) -> return $ t >= 0.0 && t < len vectorTowardsLight

flatWhite:: Material
flatWhite = flat (Colour 1.0 1.0 1.0)

combine :: Material -> Material -> Material
combine m1 m2 =
  \env ray recDepth dist normal -> do
    c1 <- m1 env ray recDepth dist normal
    c2 <- m2 env ray recDepth dist normal
    return $ blend c1 c2

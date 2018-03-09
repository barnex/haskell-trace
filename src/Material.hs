module Material where

import Data.List
import Data.Maybe
import Object
import Ray
import Shape
import Types
import Vector as Vector
import System.Random
import Control.Monad.Trans.RWS

type Material = Ray -> RecDepth -> Distance -> UnitVector -> JefDeMonad Colour

paint::Shape -> Material -> Object
paint shape material = \ray recDepth -> do 
  let maybeDistNormal = shape ray
  case maybeDistNormal of
    Just (dist, normal) -> do
      colour <- material ray recDepth dist normal
      return $ Just (dist, colour)
    Nothing -> return Nothing

flat::Colour -> Material
flat colour = \_ _ _ _ -> return colour

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

generateRandomVector:: UnitVector -> JefDeMonad UnitVector
generateRandomVector normal = do
  randomX <- state (randomR (-1.0, 1.0))
  randomY <- state (randomR (-1.0, 1.0))
  randomZ <- state (randomR (-1.0, 1.0))
  let vect = Vector.vector randomX randomY randomZ
  let squareLen = len2 vect
  if squareLen > 1 then
    generateRandomVector normal
  else
    let normalized = normalize vect in
    if normalized `dot` normal > 0 then
      return normalized
    else
      return $ normalize $ (-1.0) `vecMul` normalized 


diffuse::Colour -> Material
diffuse colour = \ray recDepth dist normal' -> do
  let normal = makeSureNormalPointsOutwards ray normal'
  let p = pointOnShape ray dist normal 
  randDirection <- generateRandomVector normal
-- Kasper extract this in a function, the indirects tuff
  let indirectRay = Ray p randDirection
  indirectColour <- findColour indirectRay $ recDepth -1 
  let indirectTheta =  normal `dot` randDirection
  let scaledColour = indirectTheta `scale` indirectColour
  envLight <- reader light 
  let v = vectorTowardsLight envLight p
  let n = normalize v 
  let rayTowardsLight = Ray p n 
  doesOcclude <- occludes rayTowardsLight v
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
specular specularPower = \ray@(Ray _ dir) _ dist normal' -> do
  let normal = makeSureNormalPointsOutwards ray normal' 
  let reflectedDir = reflected dir normal
  let p = pointOnShape ray dist normal
  envLight <- reader light
  let v = vectorTowardsLight envLight p
  let n = normalize v 
  let rayTowardsLight = Ray p n 
  let specularComponent = (reflectedDir `dot` n) ** specularPower
  doesOcclude <- occludes rayTowardsLight v
  if doesOcclude then
      return $ Colour 0.0 0.0 0.0
  else
    -- TODO: add falloff
    return $ clamp specularComponent `scale` (Colour 1.0 1.0 1.0) -- TODO: this should be the colour of the light

-- TODO: Koen: reflectivity type
reflective::Colour -> Material
reflective reflectivity = \ray@(Ray _ dir) recDepth dist normal' ->
  let normal = makeSureNormalPointsOutwards ray normal' in 
  let pointOnShape = ray `at` dist in
 -- let dir' = normalize $ vecMul (1.0) dir in -- TODO: why is this 1.0 and not -1.0 ????
  let reflectedDir = reflected dir normal in
  let reflectedRay = offset 1e-6 $ Ray pointOnShape reflectedDir in
    do
      materialColour <- findColour reflectedRay (recDepth - 1)
      return $ reflectivity `mulColour` materialColour

reflected:: UnitVector -> UnitVector -> UnitVector
reflected dir normal = normalize $ dir `vecSub` (vecMul (2.0*(normal `dot` dir)) normal)
  
occludes::Ray -> Vector -> JefDeMonad Bool
occludes ray vectorTowardsLight = do
  s <- reader scene
  let f = \o -> o ray (-1)
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
  \ray recDepth dist normal -> do
    c1 <- m1 ray recDepth dist normal
    c2 <- m2 ray recDepth dist normal
    return $ blend c1 c2

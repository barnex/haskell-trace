{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Codec.Picture
import Data.List
import Data.Maybe
import GHC.Float
import Material
import Object
import Ray
import Sphere
import Sheet
import Types as Types
import Vector
import qualified Debug.Trace as Trace

-- | render determines the color of a pixel.
render:: Env -> Int -> Int -> PixelRGBF
render env i j = 
  let (x, y) = pixelToCoordinate i j width height
      ray = rayFrom x y
      f = double2Float . srgb
      Colour r g b = findColour env ray
  in
  PixelRGBF (f r) (f g) (f b)
  

-- | findColour determines the color seen by a ray
findColour :: Env -> Ray -> Colour
findColour env ray = 
  let shapes = scene env
      f shape = shape env ray
      maybeDistancesAndColours = f <$> shapes
      sortedTs = sortOn fst $ catMaybes maybeDistancesAndColours
  in
  case sortedTs of
    [] -> backgroundColour env
    (x:xs) -> snd x


-- | rayFrom creates a ray starting from position (x, y) on the camera.
-- TODO: the focal length (2) is still hard-coded.
rayFrom:: Double -> Double -> Ray
rayFrom x y = 
  let start = (Vector x y 0.0)
      f = Vector 0 0 2
      dir = normalize $ sub start f
  in
  Ray start dir


-- | pixelToCoordinate turns a pixel index into a physical coordinate on the camera.
pixelToCoordinate:: Int -> Int -> Int -> Int -> (Double, Double)
pixelToCoordinate i j width height  = 
  let i'::Double = fromIntegral i
      j'::Double = fromIntegral j
      w'::Double = fromIntegral width
      h'::Double = fromIntegral height
      a = w'/h'
      x =  a*(2.0*(i'+0.5)/w' - 1.0)
      y = -2.0*(j'+0.5)/h' + 1.0
  in
  (x, y)

width :: Int
width = 800

height :: Int
height = 600
      
main :: IO ()
main =
  let c = Vector 0.0 0.0 (-1.0) in
  let r = 0.5 in
  let o = paint (sphere c r) $ diffuse $ Colour 1.0 0.0 0.0 in
  let c' = Vector 1.0 0.0 (-2.0) in
  let o' = paint (sphere c' r) $ diffuse $ Colour 0.0 1.0 0.0 in
  let s = paint (sheety (-1.0)) $ diffuse $ Colour 1.0 1.0 1.0 in
  let ray = Ray (Vector 1.0 2.0 0.0) (Vector 0.0 (-1.0) 0.0) in
  let env = Env{ scene = [o, o', s] , backgroundColour = Colour 0.1 0.1 0.1 } in
  do
    saveBmpImage "test.bmp" $ ImageRGBF $ generateImage (render env) width height

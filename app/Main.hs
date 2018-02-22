{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Codec.Picture
import GHC.Float
import Material
import Object
import Ray
import Sphere
import Sheet
import Types as Types
import Vector

-- | render determines the color of a pixel.
render:: Env -> Int -> Int -> PixelRGBF
render env i j = 
  let (x, y) = pixelToCoordinate i j width height
      ray = rayFrom x y
      f = double2Float . srgb
      Colour r g b = findColour env ray 10
  in
  PixelRGBF (f r) (f g) (f b)
  
-- | rayFrom creates a ray starting from position (x, y) on the camera.
-- TODO: the focal length (2) is still hard-coded.
rayFrom:: Double -> Double -> Ray
rayFrom x y = 
  let start = vector x y 0.0
      f = vector 0 0 2
      dir = normalize $ vecSub start f
  in
  Ray start dir


-- | pixelToCoordinate turns a pixel index into a physical coordinate on the camera.
pixelToCoordinate:: Int -> Int -> Int -> Int -> (Double, Double)
pixelToCoordinate i j w h  = 
  let i'::Double = fromIntegral i
      j'::Double = fromIntegral j
      w'::Double = fromIntegral w
      h'::Double = fromIntegral h
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
  let c = vector (-0.5) (-0.2) (-1.0)
      o = paint (sphere c 0.7) $ combine (specular 20.0) (diffuse $ Colour 1.0 0.0 0.0)
      c' = vector 1.0 (-0.2) (-2.0)
      o' = paint (sphere c' 1.0) $ combine (diffuse $ Colour 0.0 1.0 0.0) (reflective $ Colour 0.1 0.1 0.1)
      ceiling = paint (sheety (1.0) ) (diffuse $ Colour 1.0 1.0 1.0)
      floor = paint (sheety (-1.0) ) (diffuse $ Colour 1.0 1.0 1.0)   
      env = Env{ scene = [o, o', ceiling, floor ] , backgroundColour = Colour 0.1 0.1 0.1, light = vector 0 0.999 0.0 }
  in
  do
    saveBmpImage "test.bmp" $ ImageRGBF $ generateImage (render env) width height

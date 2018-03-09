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
import System.Random
import Control.Monad.Trans.RWS


-- | render determines the color of a pixel.
render:: Int -> Int -> JefDeMonad PixelRGBF
render i j = do
  let (x, y) = pixelToCoordinate i j width height
  let ray = rayFrom x y
  let f = double2Float . srgb
  Colour r g b <- findColour ray 10
  return $ PixelRGBF (f r) (f g) (f b)
  

-- camera definition
-- TODO: cleanup
camz::Double
camz = 1
focallen::Double
focallen = 2.4

-- | rayFrom creates a ray starting from position (x, y) on the camera.
-- TODO: the focal length (2) is still hard-coded.
rayFrom:: Double -> Double -> Ray
rayFrom x y = 
  let start = vector x y camz
      f = vector 0 0 (camz + focallen)
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
main = do
    randomGenerator <- getStdGen

    let white = (diffuse $ Colour 0.9 0.9 0.9)
    let red = (diffuse $ Colour 0.9 0.0 0.0)
    let green = (diffuse $ Colour 0.0 0.9 0.0)
    let ceiling = paint (sheety (1.0) ) white
    let floor = paint (sheety (-1.0) ) white
    let back = paint (sheetz (-2.0)) white
    let left = paint (sheetx (-1.0)) red
    let right = paint (sheetx (1.0)) green
    let c = vector (0.4) (-0.6) (0.2)
    let o = paint (sphere c 0.4) $ combine (specular 20.0) white 
    let c' = vector (-0.3) (-0.6) (-0.5)
    let o' = paint (sphere c' 0.4) $ combine (diffuse $ Colour 0.1 0.1 0.1) (reflective $ Colour 0.7 0.7 0.7)
    let env = Env{ scene = [o, o', ceiling, floor, back, left, right] , backgroundColour = Colour 0.1 0.1 0.1, light = vector 0 0.9 0.0 }

    (image, _, _) <- runRWST (withImage width height render) env randomGenerator
    saveBmpImage "test.bmp" $ ImageRGBF image

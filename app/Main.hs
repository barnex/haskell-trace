module Main where

import Types
import Codec.Picture

data Env = Env { 
           camPos :: Vector,
           focalLen :: Double
    }

render:: Env -> Int -> Int -> PixelRGBF
render e x y = PixelRGBF 1.0 1.0 1.0

width :: Int
width = 800

height :: Int
height = 600

main :: IO ()
main =
  let c = Vector 0.0 1.0 0.0 in
  let r = 1 in
  let s = Sphere c r in
  let ray = Ray (Vector 1.0 2.0 0.0) (Vector 0.0 (-1.0) 0.0) in
  let env = Env{ camPos = Vector 0.0 0.0 0.0, focalLen = 1 } in
  do
    print $ intersect s ray
    saveBmpImage "test.bmp" $ ImageRGBF $ generateImage (render env) width height

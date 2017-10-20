{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Types
import Codec.Picture

data Env = Env { 
           camPos :: Vector,
           focalLen :: Double
    }

render:: Env -> Int -> Int -> PixelRGBF
render e i j =
   	case t of
        Nothing -> PixelRGBF 1.0 1.0 1.0 
        Just _  -> PixelRGBF 1.0 0.0 0.0
    where
      i'::Double = fromIntegral i
      j'::Double = fromIntegral j
      w'::Double = fromIntegral width
      h'::Double = fromIntegral height
      a = w'/h'
      x =  a*(2.0*i'/w' - 1.0)
      y = -2.0*j'/h' + 1.0
      r = Ray (Vector x y 0.0) (Vector 0.0 0.0 (-1.0))
      s = Sphere (Vector 0.0 0.0 (-1.0)) 0.5
      t = intersect s r

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

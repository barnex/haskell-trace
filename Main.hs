module Main where

import Types


main :: IO ()
main =
  let c = Vector 0.0 1.0 0.0 in
  let r = 1 in
  let s = Sphere c r in
  let ray = Ray (Vector 1.0 2.0 0.0) (Vector 0.0 (-1.0) 0.0) in
  do print $ intersect s ray

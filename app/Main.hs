{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Types as Types
import Codec.Picture
import GHC.Float
import Data.List
import Data.Maybe
import qualified Safe as Safe
import qualified Debug.Trace as Trace

data Env = Env { 
           camPos :: Vector,
           focalLen :: Double,
           scene :: [Sphere]
    }

render:: Env -> Int -> Int -> PixelRGBF
render env i j =
   	case tsMaybe of
        Nothing -> PixelRGBF 1.0 1.0 1.0 
        Just (t, s) ->
          let p = at r t in
          let (Vector _ _ z) = normalVector s p in
          let z' = double2Float $ srgb z in
          PixelRGBF z' z' z'
    where
      i'::Double = fromIntegral i
      j'::Double = fromIntegral j
      w'::Double = fromIntegral width
      h'::Double = fromIntegral height
      a = w'/h'
      x =  a*(2.0*(i'+0.5)/w' - 1.0)
      y = -2.0*(j'+0.5)/h' + 1.0
      start = (Vector x y 0.0)
      f = Vector 0 0 2
      dir = normalize $ sub start f
      r = Ray start dir
      ts = map (\s -> (Types.intersect s r, s)) $ scene env
      sortedTs = sortOn fst ts
      filteredAndSortedTs = filter (\(t,s) ->
                             case t of
                               Just x | x > 0 -> True
                               _ -> False
                            ) sortedTs
      tsMaybe =  case filteredAndSortedTs of
                 [] -> Nothing
                 (x:xs) -> let (maybeT, s) = x in
                           Just (fromJust maybeT, s)
      

width :: Int
width = 800

height :: Int
height = 600

main :: IO ()
main =
  let c = Vector 0.0 0.0 (-1.0) in
  let r = 0.5 in
  let s = Sphere c r in
  let c' = Vector 1.0 0.0 (-2.0) in
  let s' = Sphere c' r in
  let ray = Ray (Vector 1.0 2.0 0.0) (Vector 0.0 (-1.0) 0.0) in
  let env = Env{ camPos = Vector 0.0 0.0 0.0, focalLen = 1, scene = [s, s'] } in
  do
    saveBmpImage "test.bmp" $ ImageRGBF $ generateImage (render env) width height

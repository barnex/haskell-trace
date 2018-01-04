{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Types as Types
import Codec.Picture
import GHC.Float
import Data.List
import Data.Maybe
import qualified Debug.Trace as Trace

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

render:: Env -> Int -> Int -> PixelRGBF
render env i j = 
  let (x, y) = pixelToCoordinate i j width height
      ray = rayFrom x y
      f = double2Float . srgb
      Colour r g b = findColour env ray
  in
  PixelRGBF (f r) (f g) (f b)
  
{-    case tsMaybe of
      Nothing -> PixelRGBF 1.0 1.0 1.0 
      Just (t, s) ->
        let p = at r t in
        let (Vector _ _ z) = normalVector s p in
        let z' = double2Float $ srgb z in
        PixelRGBF z' z' z'
    where
      (x, y) = pixelToCoordinate i j width height
      r = rayFrom x y
      tsMaybe = findIntersect r $ scene env-}
 

{-findIntersect :: Ray -> [Sphere] -> Maybe (Double, Sphere) 
findIntersect r scene = 
  let ts = map (\s -> (Types.intersect s r, s)) scene
      sortedTs = sortOn fst ts
      filteredAndSortedTs = filter (\(t,s) ->
                         case t of
                           Just x | x > -1 -> True
                           _ -> False
                        ) sortedTs
  in
  case filteredAndSortedTs of
    [] -> Nothing
    (x:xs) -> let (maybeT, s) = x in
              Just (fromJust maybeT, s)-}

rayFrom:: Double -> Double -> Ray
rayFrom x y = 
  let start = (Vector x y 0.0)
      f = Vector 0 0 2
      dir = normalize $ sub start f
  in
  Ray start dir
      
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
  let s = sphere c r in
  let c' = Vector 1.0 0.0 (-2.0) in
  let s' = sphere c' r in
  let ray = Ray (Vector 1.0 2.0 0.0) (Vector 0.0 (-1.0) 0.0) in
  let env = Env{ scene = [s, s'] , backgroundColour = Colour 0.0 0.0 0.0 } in
  do
    saveBmpImage "test.bmp" $ ImageRGBF $ generateImage (render env) width height

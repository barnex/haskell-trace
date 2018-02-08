module Object where

import Data.Maybe
import Data.List
import Ray
import Types
import Vector

type Object = Env -> Ray -> Maybe (Distance, Colour)

data Env = Env { 
           scene :: [Object],
           backgroundColour::Colour
    }

--
-- | findColour determines the color seen by a ray
findColour :: Env -> Ray -> Int -> Colour
findColour env ray 0 = Colour 0.0 0.0 0.0
findColour env ray depth = 
  let shapes = scene env
      f shape = shape env ray
      maybeDistancesAndColours = f <$> shapes
      sortedTs = sortOn fst $ catMaybes maybeDistancesAndColours
  in
  case sortedTs of
    [] -> backgroundColour env
    (x:xs) -> snd x


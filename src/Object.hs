module Object where

import Data.Maybe
import Data.List
import Ray
import Types
import Vector

type Object = Env -> Ray -> RecDepth -> Maybe (Distance, Colour)

data Env = Env { 
           scene :: [Object],
           backgroundColour::Colour,
           light :: Vector
    }

--
-- | findColour determines the color seen by a ray
findColour :: Env -> Ray -> RecDepth -> Colour
--findColour env ray 0 = Colour 0.0 0.0 0.0
findColour env ray recDepth = 
  if recDepth > 0 then
    let shapes = scene env
        f shape = shape env ray recDepth
        maybeDistancesAndColours = f <$> shapes
        sortedTs = sortOn fst $ catMaybes maybeDistancesAndColours
    in
    case sortedTs of
      [] -> backgroundColour env
      (x:_) -> snd x
  else if recDepth < 0 then undefined else Colour 0.0 0.0 0.0
    


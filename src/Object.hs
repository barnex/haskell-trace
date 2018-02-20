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
findColour :: Env -> Ray -> RecDepth -> Bool -> (Distance, Colour)
--findColour env ray 0 = Colour 0.0 0.0 0.0
findColour env ray recDepth useBackgroundColour = 
  if recDepth > 0 then
    let shapes = scene env
        f shape = shape env ray recDepth
        maybeDistancesAndColours = f <$> shapes
        sortedTs = sortOn fst $ catMaybes maybeDistancesAndColours
    in
    case sortedTs of
      [] -> if useBackgroundColour then (0.0, backgroundColour env) else (1e100, Colour 0.0 0.0 0.0)
      (x:_) -> x
  else if recDepth < 0 then undefined else (0.0, Colour 0.0 0.5 0.0)
    


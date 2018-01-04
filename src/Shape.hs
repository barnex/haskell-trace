module Shape where

import Vector
import Ray

data Env = Env { 
           scene :: [Shape],
           backgroundColour::Colour
    }

type Distance = Double
data Colour = Colour Double Double Double
type Shape = Env -> Ray -> Maybe (Distance, Colour)

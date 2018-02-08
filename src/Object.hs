module Object where

import Vector
import Types
import Ray

type Object = Env -> Ray -> Maybe (Distance, Colour)

data Env = Env { 
           scene :: [Object],
           backgroundColour::Colour,
           light :: Vector
    }


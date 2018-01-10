module Shape where

import Ray
import Vector
import Types
import Object

type Shape = Env -> Ray -> Maybe (Distance, UnitVector)



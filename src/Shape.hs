module Shape where

import Ray
import Vector
import Types

type Shape = Ray -> Maybe (Distance, UnitVector)



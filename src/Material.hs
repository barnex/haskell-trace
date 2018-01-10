
module Material where

import Vector
import Ray

type UnitVector = Vector

type Material = Env -> Ray -> Distance -> UnitVector -> Colour

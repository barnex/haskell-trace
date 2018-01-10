
module Material where

import Vector
import Ray
import Types
import Shape
import Object


type Material = Env -> Ray -> Distance -> UnitVector -> Colour

paint::Shape -> Material -> Object
paint shape material = undefined

white::Material
white env ray dist norm =
    Colour 1.0 1.0 1.0

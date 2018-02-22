module Types where

data Colour = Colour Double Double Double

scale::Double -> Colour -> Colour
scale d (Colour r g b) = Colour (d*r) (d*g) (d*b)

mulColour :: Colour -> Colour -> Colour
mulColour (Colour r1 g1 b1) (Colour r2 g2 b2) =
  Colour (r1*r2) (g1*g2) (b1*b2)
  
srgb::Double -> Double
srgb x =
  if x <= 0.0031308 then 12.92 * x else 1.055*(x ** (1.0 / 2.4)) - 0.05

blend :: Colour -> Colour -> Colour 
blend (Colour r1 g1 b1) (Colour r2 g2 b2) =
  Colour (r1+r2) (g1+g2) (b1+b2)
type Distance = Double
type RecDepth = Int

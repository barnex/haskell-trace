module Types where

data Colour = Colour Double Double Double

srgb::Double -> Double
srgb x =
	if x <= 0.0031308 then 12.92 * x else 1.055*(x ** (1.0 / 2.4)) - 0.05

type Distance = Double

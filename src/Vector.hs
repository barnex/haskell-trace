module Vector where

data Vector = Vector Double Double Double deriving (Show, Eq)
type UnitVector = Vector

dot::Vector -> Vector -> Double
dot (Vector ax ay az) (Vector bx by bz) = ax*bx + ay*by + az*bz

len2::Vector -> Double
len2 v = dot v v

len::Vector -> Double
len v = sqrt $ len2 v

vectorSum::Vector -> Vector -> Vector
vectorSum (Vector ax ay az) (Vector bx by bz) = Vector (ax+bx) (ay+by) (az+bz)

sub::Vector -> Vector -> Vector
sub (Vector ax ay az) (Vector bx by bz) = Vector (ax-bx) (ay-by) (az-bz)

mul:: Double -> Vector -> Vector
mul multiplicator (Vector x y z) =
  Vector (multiplicator * x) (multiplicator * y) (multiplicator * z)

normalize::Vector -> Vector
normalize v@(Vector x y z) = Vector (x/l) (y/l) (z/l)
  where
    l = len v

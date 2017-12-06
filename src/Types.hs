module Types where

data Vector = Vector Double Double Double deriving Show

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


-- start dir
data Ray = Ray Vector Vector deriving Show

at:: Ray -> Double -> Vector
at (Ray startVector direction) t =
  vectorSum startVector $ mul t direction

type Center = Vector
type Radius = Double
data Sphere = Sphere Center Radius deriving Show

intersect::Sphere -> Ray -> Maybe Double
intersect s ray =
  let Sphere c r = s in
  let Ray start dir = ray in
  let v = sub start c in
  let discr = (dot v dir ^^ 2) - len2 v +  r ^^ 2 in
  if discr < 0 then Nothing else Just $ (-(dot v dir)) - sqrt discr

normalVector :: Sphere -> Vector -> Vector
normalVector (Sphere center _) point =
  normalize $ sub point center

srgb::Double -> Double
srgb x =
	if x <= 0.0031308 then 12.92 * x else 1.055*(x ** (1.0 / 2.4)) - 0.05

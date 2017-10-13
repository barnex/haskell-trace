module Types where

data Vector = Vector Double Double Double deriving Show

dot::Vector -> Vector -> Double
dot (Vector ax ay az) (Vector bx by bz) = ax*bx + ay*by + az*bz

len2::Vector -> Double
len2 v = dot v v

sum::Vector -> Vector -> Vector
sum (Vector ax ay az) (Vector bx by bz) = Vector (ax+bx) (ay+by) (az+bz)

sub::Vector -> Vector -> Vector
sub (Vector ax ay az) (Vector bx by bz) = Vector (ax-bx) (ay-by) (az-bz)


data Ray = Ray Vector Vector deriving Show


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



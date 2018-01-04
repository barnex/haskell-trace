module Types where

-- start dir

{-sphere s ray =
  let Sphere c r = s in
  let Ray start dir = ray in
  let v = sub start c in
  let discr = (dot v dir ^^ 2) - len2 v +  r ^^ 2 in
  if discr < 0 then Nothing else Just $ (-(dot v dir)) - sqrt discr-}

{-normalVector :: Sphere -> Vector -> Vector
normalVector (Sphere center _) point =
  normalize $ sub point center-}

srgb::Double -> Double
srgb x =
	if x <= 0.0031308 then 12.92 * x else 1.055*(x ** (1.0 / 2.4)) - 0.05

{-# LANGUAGE GADTs #-}
module Vector where

data UnitLength
data AnyLength

data VectorGADT a where 
    Any :: (Double, Double, Double) -> VectorGADT AnyLength
    Unit :: (Double, Double, Double) -> VectorGADT UnitLength 

type UnitVector = VectorGADT UnitLength
type Vector = VectorGADT AnyLength

instance Show (VectorGADT a) where
  show (Any v) = show v
  show (Unit v) = show v

vector :: Double -> Double -> Double -> VectorGADT AnyLength
vector x y z =
  Any (x,y,z)

toAny :: VectorGADT a -> VectorGADT AnyLength
toAny (Any v) = Any v
toAny (Unit v) = Any v

elements :: VectorGADT a -> (Double, Double, Double)
elements v =
  let Any xyz = toAny v in
  xyz

dot :: VectorGADT a -> VectorGADT b -> Double
dot v1 v2 = 
  let Any (ax, ay, az) = toAny v1 
      Any (bx, by, bz) = toAny v2
  in    
  ax*bx + ay*by + az*bz

len2 :: VectorGADT a -> Double
len2 v = dot v v

len :: VectorGADT a -> Double
len v = sqrt $ len2 v

vecSum :: VectorGADT a -> VectorGADT b -> VectorGADT AnyLength
vecSum v1 v2 =
  let Any (ax, ay, az) = toAny v1 
      Any (bx, by, bz) = toAny v2
  in 
  Any (ax+bx, ay+by, az+bz)

vecSub :: VectorGADT a -> VectorGADT b -> VectorGADT AnyLength
vecSub v1 v2 =
  let Any (ax, ay, az) = toAny v1 
      Any (bx, by, bz) = toAny v2
  in 
  Any (ax-bx, ay-by, az-bz)

vecMul:: Double -> VectorGADT a -> VectorGADT AnyLength
vecMul multiplicator v =
  let Any (x, y, z) = toAny v in 
  Any (multiplicator * x, multiplicator * y, multiplicator * z)

normalize::VectorGADT a -> VectorGADT UnitLength
normalize v@(Unit _) = v
normalize v@(Any (x,y,z)) =
  Unit (x/l, y/l, z/l)
  where
    l = len v

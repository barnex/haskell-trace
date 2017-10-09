module TypesSpec where

import Test.Hspec
import Types

spec :: Spec
spec = 
  describe "intersection of sphere and ray" $ do
    it "should be 1.0 for Vector 1 2 and Sphere (0 1) 1" $
      let c = Vector 0.0 1.0 0.0 in
      let r = 1 in
      let s = Sphere c r in
      let ray = Ray (Vector 1.0 2.0 0.0) (Vector 0.0 (-1.0) 0.0) in
      do
        intersect s ray  `shouldBe` (Just 1.0)

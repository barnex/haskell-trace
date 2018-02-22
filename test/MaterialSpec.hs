module MaterialSpec where

import Material
import Vector
import Test.Hspec

spec :: Spec
spec = 
  describe "weird formula" $ do
    it "is not knocking like a bus" $
      let dir' = normalize $ vector 0.0 1.0 2.0 in
      let normal = normalize $ vector 1.0 1.0 1.0 in
      reflected dir' normal `shouldBe` Unit (-0.8944271909999159, -0.44721359549995804,-2.2204460492503126e-16)

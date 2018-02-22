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
      reflected dir' normal `shouldBe` normalize (vector 5.0 6.0 7.0)

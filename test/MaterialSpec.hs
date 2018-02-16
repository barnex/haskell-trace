module MaterialSpec where

import Material
import Vector
import Test.Hspec

spec :: Spec
spec = 
  describe "weird formula" $ do
    it "is not knocking like a bus" $
      let dir' = Vector 0.0 1.0 2.0 in
      let normal = Vector 1.0 1.0 1.0 in
      confusingFormula dir' normal `shouldBe` Vector 5.0 6.0 7.0
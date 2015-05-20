module ScadDSLSpec where
import Test.Hspec
import Data.Tree

import ScadDSL

spec :: Spec
spec = do
    describe "forestToStrings" $ do
      it "single cube" $ do
        let forest = [Node (Cube 1.0) []]
        forestToStrings forest `shouldBe` ["cube(1.0);"]
      it "single cylinder" $ do
        let forest = [Node (Cylinder 10.0 2.0 True) []]
        forestToStrings forest `shouldBe` ["cylinder(10.0, 2.0, center=True);"]

main :: IO ()
main = hspec spec

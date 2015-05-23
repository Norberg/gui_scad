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
      context "single cylinder" $ do
        it "r, center = True" $ do
            let forest = [Node (Cylinder 10.0 (Radius 2.0) Nothing True) []]
            forestToStrings forest `shouldBe`
                ["cylinder(h = 10.0, r = 2.0, center = True);"]
        it "d, center = false" $ do
            let forest = [Node (Cylinder 2.5 (Diameter 3.3) Nothing False) []]
            forestToStrings forest `shouldBe`
                ["cylinder(h = 2.5, d = 3.3, center = False);"]
        it "r1, r2, center = True" $ do
            let forest = [Node (Cylinder 2.5 (Radius 3.3) (Just $ Radius 4.5) True) []]
            forestToStrings forest `shouldBe`
                ["cylinder(h = 2.5, r1 = 3.3, r2 = 4.5, center = True);"]
        it "d1, d2, center = False" $ do
            let forest = [Node (Cylinder 2.5 (Diameter 3.3) (Just $ Diameter 4.5) False) []]
            forestToStrings forest `shouldBe`
                ["cylinder(h = 2.5, d1 = 3.3, d2 = 4.5, center = False);"]
        it "d1, r2, center = False" $ do
            let forest = [Node (Cylinder 2.5 (Diameter 3.3) (Just $ Radius 4.5) False) []]
            forestToStrings forest `shouldBe`
                ["cylinder(h = 2.5, d1 = 3.3, r2 = 4.5, center = False);"]
      context "single sphere" $ do
        it "radius" $ do
            let forest = [Node (Sphere (Radius 2.0)) []]
            forestToStrings forest `shouldBe` ["sphere(2.0);"]
        it "diameter" $ do
            let forest = [Node (Sphere (Diameter 1.0)) []]
            forestToStrings forest `shouldBe` ["sphere(d = 1.0);"]

main :: IO ()
main = hspec spec

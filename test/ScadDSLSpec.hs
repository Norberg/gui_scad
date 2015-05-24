{-# LANGUAGE OverloadedStrings #-}
module ScadDSLSpec where
import Test.Hspec
import Data.Tree
import qualified Data.Text as T

import ScadDSL

spec :: Spec
spec = do
    describe "forestToText" $ do
      context "single cube" $ do
        it "with size" $ do
            let forest = [Node (Cube $ Size 1.0) []]
            forestToText forest `shouldBe` "cube(1.0);"
        it "with dimension" $ do
            let forest = [Node (Cube $ Dimension 1.0 2.0 3.0) []]
            forestToText forest `shouldBe` "cube([1.0, 2.0, 3.0]);"
      context "single cylinder" $ do
        it "with r, center = True" $ do
            let forest = [Node (Cylinder 10.0 (Radius 2.0) Nothing True) []]
            forestToText forest `shouldBe`
                "cylinder(h = 10.0, r = 2.0, center = true);"
        it "with d, center = false" $ do
            let forest = [Node (Cylinder 2.5 (Diameter 3.3) Nothing False) []]
            forestToText forest `shouldBe`
                "cylinder(h = 2.5, d = 3.3, center = false);"
        it "with r1, r2, center = True" $ do
            let forest = [Node (Cylinder 2.5 (Radius 3.3) (Just $ Radius 4.5) True) []]
            forestToText forest `shouldBe`
                "cylinder(h = 2.5, r1 = 3.3, r2 = 4.5, center = true);"
        it "with d1, d2, center = False" $ do
            let forest = [Node (Cylinder 2.5 (Diameter 3.3) (Just $ Diameter 4.5) False) []]
            forestToText forest `shouldBe`
                "cylinder(h = 2.5, d1 = 3.3, d2 = 4.5, center = false);"
        it "with d1, r2, center = False" $ do
            let forest = [Node (Cylinder 2.5 (Diameter 3.3) (Just $ Radius 4.5) False) []]
            forestToText forest `shouldBe`
                "cylinder(h = 2.5, d1 = 3.3, r2 = 4.5, center = false);"
      context "single sphere" $ do
        it "with radius" $ do
            let forest = [Node (Sphere (Radius 2.0)) []]
            forestToText forest `shouldBe` "sphere(2.0);"
        it "with diameter" $ do
            let forest = [Node (Sphere (Diameter 1.0)) []]
            forestToText forest `shouldBe` "sphere(d = 1.0);"
      context "translate" $ do
        it "single translate" $ do
            let forest = [Node (Translate 1.0 2.0 3.0) []]
            forestToText forest `shouldBe` "translate(1.0, 2.0, 3.0);"
    {--    it "nested translate" $ do
            let forest = [Node (Translate 1.0 2.0 3.0) [Node (Translate 4.0 5.0 6.0) []]]
            forestToText forest `shouldBe` "translate(1.0, 2.0, 3.0)"
--}
main :: IO ()
main = hspec spec

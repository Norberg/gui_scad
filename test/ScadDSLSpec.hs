{-# LANGUAGE OverloadedStrings #-}
module ScadDSLSpec where
import Test.Hspec
import Data.Tree

import DSL.Scad
import DSL.Generate

spec :: Spec
spec = do
    describe "forestToText" $ do
      context "single cube" $ do
        it "with size" $ do
            let forest = [Node (Cube $ Size 1.0) []]
            forestToText forest `shouldBe` "cube(1.0);\n"
        it "with dimension" $ do
            let forest = [Node (Cube $ Dimension 1.0 2.0 3.0) []]
            forestToText forest `shouldBe` "cube([1.0, 2.0, 3.0]);\n"
      context "single cylinder" $ do
        it "with r, center = True" $ do
            let forest = [Node (Cylinder 10.0 (Radius 2.0) Nothing True) []]
            forestToText forest `shouldBe`
                "cylinder(h = 10.0, r = 2.0, center = true);\n"
        it "with d, center = false" $ do
            let forest = [Node (Cylinder 2.5 (Diameter 3.3) Nothing False) []]
            forestToText forest `shouldBe`
                "cylinder(h = 2.5, d = 3.3, center = false);\n"
        it "with r1, r2, center = True" $ do
            let forest = [Node (Cylinder 2.5 (Radius 3.3) (Just $ Radius 4.5) True) []]
            forestToText forest `shouldBe`
                "cylinder(h = 2.5, r1 = 3.3, r2 = 4.5, center = true);\n"
        it "with d1, d2, center = False" $ do
            let forest = [Node (Cylinder 2.5 (Diameter 3.3) (Just $ Diameter 4.5) False) []]
            forestToText forest `shouldBe`
                "cylinder(h = 2.5, d1 = 3.3, d2 = 4.5, center = false);\n"
        it "with d1, r2, center = False" $ do
            let forest = [Node (Cylinder 2.5 (Diameter 3.3) (Just $ Radius 4.5) False) []]
            forestToText forest `shouldBe`
                "cylinder(h = 2.5, d1 = 3.3, r2 = 4.5, center = false);\n"
      context "single sphere" $ do
        it "with radius" $ do
            let forest = [Node (Sphere (Radius 2.0)) []]
            forestToText forest `shouldBe` "sphere(2.0);\n"
        it "with diameter" $ do
            let forest = [Node (Sphere (Diameter 1.0)) []]
            forestToText forest `shouldBe` "sphere(d = 1.0);\n"
      context "translate" $ do
        it "single translate" $ do
            let forest = [Node (Translate 1.0 2.0 3.0) []]
            forestToText forest `shouldBe` "translate(1.0, 2.0, 3.0);\n"
        it "nested translate" $ do
            let forest = [Node (Translate 1.0 2.0 3.0) [Node (Translate 4.0 5.0 6.0) []]]
            forestToText forest `shouldBe`
                "translate(1.0, 2.0, 3.0)\n{\n    translate(4.0, 5.0, 6.0);\n}\n"
      context "rotate" $ do
        it "single rotate" $ do
            let forest = [Node (Rotate 1.0 2.0 3.0) []]
            forestToText forest `shouldBe` "rotate(1.0, 2.0, 3.0);\n"
        it "nested rotate" $ do
            let forest = [Node (Rotate 1.0 2.0 3.0) [Node (Rotate 4.0 5.0 6.0) []]]
            forestToText forest `shouldBe`
                "rotate(1.0, 2.0, 3.0)\n{\n    rotate(4.0, 5.0, 6.0);\n}\n"
      context "union" $ do
        it "single union" $ do
            let forest = [Node Union []]
            forestToText forest `shouldBe` "union();\n"
        it "nested union" $ do
            let forest = [Node Union [Node Union []]]
            forestToText forest `shouldBe`
                "union()\n{\n    union();\n}\n"
      context "difference" $ do
        it "single difference" $ do
            let forest = [Node Difference []]
            forestToText forest `shouldBe` "difference();\n"
        it "nested difference" $ do
            let forest = [Node Difference [Node Difference []]]
            forestToText forest `shouldBe`
                "difference()\n{\n    difference();\n}\n"
      context "intersection" $ do
        it "single intersection" $ do
            let forest = [Node Intersection []]
            forestToText forest `shouldBe` "intersection();\n"
        it "nested intersection" $ do
            let forest = [Node Intersection [Node Intersection []]]
            forestToText forest `shouldBe`
                "intersection()\n{\n    intersection();\n}\n"

main :: IO ()
main = hspec spec

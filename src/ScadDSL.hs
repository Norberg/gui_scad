{-# LANGUAGE OverloadedStrings #-}
module ScadDSL
( Scad(..),
  Distance(..),
  CubeSize(..),
  forestToText
)
where

import Data.Tree
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.RealFloat
import Data.Monoid (mconcat, (<>))


data Scad = Sphere Distance
          | Cube CubeSize
          | Cylinder Height Distance (Maybe Distance) Center
          | Union
          | Difference
          | Intersection
          | Translate X Y Z
          | Rotate X Y Z
          | Root deriving(Show, Read)


data CubeSize = Size Float | Dimension Width Depth Height deriving(Show, Read) 
data Distance = Radius Float | Diameter Float deriving(Show, Read)
type Width = Float
type Height = Float
type Depth = Float
type Center = Bool
type X = Float
type Y = Float
type Z = Float


forestToText :: Forest Scad -> T.Text
forestToText forest = mconcat $ map (toStrict . toLazyText . buildTree) forest

-- cube(size);
buildTree :: Tree Scad -> Builder
buildTree (Node (Cube size) tree) = "cube(" <> buildCubeSize size <> ");"

-- cylinder(h, r|d, center);
buildTree (Node (Cylinder h distance Nothing center) tree) =
    "cylinder(h = " <> buildFloat h <> 
    ", " <> buildSingleDistance distance <>
    ", center = " <> buildBool center <> ");"

-- cylinder(h, r1|d1, r2|d2, center);
buildTree (Node (Cylinder h distance1 (Just distance2) center) tree) =
    "cylinder(h = " <> buildFloat h <> 
    ", " <> buildDistance1 distance1 <> 
    ", " <> buildDistance2 distance2 <> 
    ", center = " <> buildBool center <> ");"

-- sphere(radius | d=diameter);
buildTree (Node (Sphere distance) tree) =
    "sphere(" <> buildSphereDistance distance <>
    ");"

-- translate(x,y,z)
buildTree (Node (Translate x y z) tree) =
    "translate(" <> buildFloat x <>
    ", " <> buildFloat y <>
    ", " <> buildFloat z <>
    ");"

buildSphereDistance :: Distance -> Builder
buildSphereDistance (Radius r) = buildFloat r
buildSphereDistance (Diameter d) = "d = " <> buildFloat d

buildSingleDistance :: Distance -> Builder
buildSingleDistance (Radius r) = "r = " <> buildFloat r
buildSingleDistance (Diameter d) = "d = " <> buildFloat d

buildDistance1 :: Distance -> Builder
buildDistance1 (Radius r1) = "r1 = " <> buildFloat r1
buildDistance1 (Diameter d1) = "d1 = " <> buildFloat d1

buildDistance2 :: Distance -> Builder
buildDistance2 (Radius r2) = "r2 = " <> buildFloat r2
buildDistance2 (Diameter d2) = "d2 = " <> buildFloat d2

buildCubeSize :: CubeSize -> Builder
buildCubeSize (Size size) = buildFloat size
buildCubeSize (Dimension width depth height) = 
    "[" <> buildFloat width <>
    ", " <> buildFloat depth <>
    ", " <> buildFloat height <> 
    "]"

buildFloat :: Float -> Builder
buildFloat float = formatRealFloat Fixed Nothing float

buildBool :: Bool -> Builder
buildBool True = fromText "true" 
buildBool False = fromText "false" 

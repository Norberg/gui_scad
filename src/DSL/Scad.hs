{-# LANGUAGE OverloadedStrings #-}
module DSL.Scad
( Scad(..),
  Distance(..),
  CubeSize(..),
)
where

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

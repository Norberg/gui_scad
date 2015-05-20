module ScadDSL
( Scad(..),
  forestToStrings
)
where

import Data.Tree

data Scad = Sphere Radius
          | Cube Size
          | Cylinder Height Radius Center
          | Union
          | Difference
          | Intersection
          | Translate X Y Z
          | Rotate X Y Z 
          | Root deriving(Show, Read)


type Radius = Float
type Size = Float
type Height = Float
type Center = Bool
type X = Float
type Y = Float
type Z = Float


forestToStrings :: Forest Scad -> [String]
forestToStrings forest = map treeToStrings forest

treeToStrings :: Tree Scad -> String
treeToStrings (Node (Cube size) xs) = "cube(" ++ show size ++ ");"
treeToStrings (Node (Cylinder h r True) xs) =
     "cylinder(" ++ show h ++ ", " ++ show r ++", center=True);"

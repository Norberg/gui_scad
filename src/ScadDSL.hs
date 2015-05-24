module ScadDSL
( Scad(..),
  Distance(..),
  CubeSize(..),
  forestToStrings
)
where

import Data.Tree

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


forestToStrings :: Forest Scad -> [String]
forestToStrings forest = map treeToStrings forest

-- cube(size);
treeToStrings :: Tree Scad -> String
treeToStrings (Node (Cube size) tree) = "cube(" ++ cubeSizeToString size ++ ");"

-- cylinder(h, r|d, center);
treeToStrings (Node (Cylinder h distance Nothing center) tree) =
    "cylinder(h = " ++ show h ++ 
    ", " ++ singleDistanceToString distance ++ 
    ", center = " ++ show center ++ ");"

-- cylinder(h, r1|d1, r2|d2, center);
treeToStrings (Node (Cylinder h distance1 (Just distance2) center) tree) =
    "cylinder(h = " ++ show h ++ 
    ", " ++ distance1ToString distance1 ++ 
    ", " ++ distance2ToString distance2 ++ 
    ", center = " ++ show center ++ ");"

-- sphere(radius | d=diameter);
treeToStrings (Node (Sphere distance) tree) =
    "sphere(" ++ sphereDistanceToString distance ++
    ");"

-- translate(x,y,z)
treeToStrings (Node (Translate x y z) tree) =
    "translate(" ++ show x ++
    ", " ++ show y ++
    ", " ++ show z ++
    ");"

sphereDistanceToString :: Distance -> String
sphereDistanceToString (Radius r) = show r
sphereDistanceToString (Diameter d) = "d = " ++ show d

singleDistanceToString :: Distance -> String
singleDistanceToString (Radius r) = "r = " ++ show r
singleDistanceToString (Diameter d) = "d = " ++ show d

distance1ToString :: Distance -> String
distance1ToString (Radius r1) = "r1 = " ++ show r1
distance1ToString (Diameter d1) = "d1 = " ++ show d1

distance2ToString :: Distance -> String
distance2ToString (Radius r2) = "r2 = " ++ show r2
distance2ToString (Diameter d2) = "d2 = " ++ show d2

cubeSizeToString :: CubeSize -> String
cubeSizeToString (Size size) = show size
cubeSizeToString (Dimension width depth height) = 
    "[" ++ show width ++
    ", " ++ show depth ++
    ", " ++ show height ++
    "]"

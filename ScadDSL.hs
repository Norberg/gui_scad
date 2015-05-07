module ScadDSL
( Scad(..)
)
where

data Scad = Sphere Radius
          | Cube Size
          | Cylinder Height Radius Center
          | Union
          | Difference
          | Intersection
          | Translate X Y Z
          | Rotate X Y Z deriving(Show)


type Radius = Float
type Size = Float
type Height = Float
type Center = Bool
type X = Float
type Y = Float
type Z = Float

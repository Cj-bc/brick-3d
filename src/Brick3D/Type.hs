{-# LANGUAGE TemplateHaskell #-}
module Brick3D.Type (
  -- * Basic type aliases
  Position
  , Rotation
  , DCPosition
  , Normal
  
  -- * Vertices
  , Vertex(Vertex)
  , v_position
  , DCVertex(DCVertex)
  , dcv_position
  , zBuffer
  , fromVertex

  -- * Primitives
  , PrimitiveBase(..)
  , Primitive
  , calcNormal
  , DCPrimitive(DCPrimitive)
  , unPrimitive
  , normal

  -- * Utility functions
  , vertices
  , toDCPrimitive

) where

import Linear.V3 (V3(..), cross, _x, _y, _z)
import Linear.V2 (V2(..))
import Linear.Matrix (M33(..))
import Lens.Micro.Platform


-- | Position in 3-Dimension
type Position = V3 Float
-- | Rotation in 3-Dimension
type Rotation = M33 Float
-- | Position in Device Coordinate
type DCPosition = V2 Float
-- | Normal of some 3-Dimensional object
type Normal   = V3 Float


-- | One 'Vertex'
data Vertex = Vertex { _v_position :: Position
                     -- We might add more information as Vertex attribute
                     } deriving (Show, Eq, Ord)
makeLenses ''Vertex

-- | 'Vertex' that is mapped to Device Coordinate
data DCVertex = DCVertex { _dcv_position :: DCPosition
                         -- | Stores depth Z value of this primitive
                         , _zBuffer :: Float
                         } deriving (Show, Eq, Ord)
makeLenses ''DCVertex

-- | Convert 'Vertex' to 'DCVertex'
fromVertex :: Vertex -> DCVertex
fromVertex v = DCVertex (V2 (v^.v_position._x) (v^.v_position._y)) (v^.v_position._z)


-- | Define Primitive shapes.
-- This would
data PrimitiveBase vtype = Point vtype
                         -- | Line (Vector vtype)
                         | Triangle vtype vtype vtype
                         deriving (Show, Eq, Ord)
makeLenses ''PrimitiveBase

-- | Primitive shape that in 3D world
type Primitive = PrimitiveBase Vertex

-- | Calculate 'Normal' of 'Primitive'
calcNormal :: Primitive -> Normal
calcNormal (Point p) = p^.v_position
calcNormal (Triangle v1 v2 _) = (v1^.v_position) `cross` (v2^.v_position)

-- | 'Primitive' that is shaded on Device Coordinate
data DCPrimitive = DCPrimitive { _unPrimitive :: PrimitiveBase DCVertex
                               , _normal :: Normal
                               }
makeLenses ''DCPrimitive


-- | 'Traversal' for evry 'Vertex' in each 'Primitive'
vertices :: Traversal' Primitive Vertex
vertices f (Point v) = Point <$> f v
vertices f (Triangle v1 v2 v3) = Triangle <$> f v1 <*> f v2 <*> f v3

-- | __THIS ISN'T 'Traversal'__
--
-- Convert 'Primitive' to 'DCPrimitive'
toDCPrimitive :: (Vertex -> DCVertex) -> Primitive -> DCPrimitive
toDCPrimitive f p@(Point v) = let norm = calcNormal p
                              in DCPrimitive (Point (f v)) norm
toDCPrimitive f tri@(Triangle v1 v2 v3) = let norm = calcNormal tri
                                          in DCPrimitive (Triangle (f v1) (f v2) (f v3)) norm

{-# LANGUAGE TemplateHaskell #-}
module Brick3D.Type (
  -- * Basic type aliases
  Position
  , Rotation
  , DCPosition
  , SCPosition
  , Normal
  
  -- * Vertices
  , Vertex(Vertex)
  , v_position
  , DCVertex(DCVertex)
  , dcv_position
  , zBuffer
  , fromVertex
  , SCVertex(SCVertex)
  , scv_position
  , depth

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
import Brick3D.Type.Base
import Brick3D.Type.Vertex

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
toDCPrimitive :: Applicative f => (Vertex -> f DCVertex) -> Primitive -> f DCPrimitive
toDCPrimitive f p@(Point v) = let norm = calcNormal p
                              in flip DCPrimitive norm <$> (Point <$> f v)
toDCPrimitive f tri@(Triangle v1 v2 v3) = let norm = calcNormal tri
                                          in flip DCPrimitive norm <$> (Triangle <$> f v1 <*> f v2 <*> f v3)

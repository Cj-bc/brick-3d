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
--
-- Device coordnate is a Coordinate that
-- put (0,0) on the center, and Y axis
-- is positive above the center. X axis
-- is positive on the right.
-- The value have range of -1~1
-- and floating number(As shown in the Type
-- definition)

-- This is the same as what OpenGL explains
-- <<https://open.gl/drawing#Vertex input>>
-- 
-- デバイス座標系は,画面の中心を原点とする
-- 座標系で, 原点から上に向かってY軸が伸び,
-- 右に向かってX軸が伸びています。
-- 値は-1~1の間の小数を取ります。
type DCPosition = V2 Float
-- | Normal of some 3-Dimensional object
type Normal   = V3 Float


-- | One 'Vertex' in 3D coordinate
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
fromVertex v | v^.v_position._z == 0 = DCVertex dcposition 0
             | otherwise            = DCVertex dcposition (abs $ 1 / v^.v_position._z)
             where
               dcposition = V2 (v^.v_position._x) (v^.v_position._y)

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

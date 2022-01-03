{-# LANGUAGE TemplateHaskell #-}
module Brick3D.Type where
import Linear.V3 (V3(..), cross, _x, _y, _z)
import Linear.V2 (V2(..))
import Linear.Matrix (M33(..))
import Lens.Micro.Platform

type Position = V3 Float
type Rotation = M33 Float
-- | Position in Device Coordinate
type DCPosition = V2 Float
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


data Primitive = Point Vertex
               -- | Line (Vector Vertex)
               | Triangle Vertex Vertex Vertex
               deriving (Show, Eq, Ord)
makeLenses ''Primitive

-- | Calculate 'Normal' of 'Primitive'
calcNormal :: Primitive -> Normal
calcNormal (Point p) = p^.v_position
calcNormal (Triangle v1 v2 _) = (v1^.v_position) `cross` (v2^.v_position)

-- | Primitive that is shaded on Device Coordinate
data DCPrimitive = DCPrimitive { _unPrimitive :: Primitive
                               , _normal :: Normal
                               }
makeLenses ''DCPrimitive

-- | Traversal for evry 'Vertex' in each 'Primitive'
-- vertices pure `shouldBe` pure 
vertices :: Traversal' Primitive Vertex
vertices f (Point v) = Point <$> f v
vertices f (Triangle v1 v2 v3) = Triangle <$> f v1 <*> f v2 <*> f v3

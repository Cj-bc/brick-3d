{-# LANGUAGE TemplateHaskell #-}
module Brick3D.Type where
import Linear.V3 (V3(..), cross)
import Lens.Micro.Platform

-- | Now it's represented as 3-D Vector
-- whereas 4-D Vector is prefered.
-- I'm doing this with reason, I firstly want to try
-- really basic way that I learnt and change it later.
type Position = V3 Float
type Rotation = V3 Float
type Normal   = V3 Float

-- | One 'Vertex'
data Vertex = Vertex { _v_normal :: Normal
                     -- We might add more information as Vertex attribute
                     } deriving (Show, Eq, Ord)
makeLenses ''Vertex

data Primitive = Point Vertex
               -- | Line (Vector Vertex)
               | Triangle Vertex Vertex Vertex
               deriving (Show, Eq, Ord)
makeLenses ''Primitive

-- | Calculate 'Normal' of 'Primitive'
calcNormal :: Primitive -> Normal
calcNormal (Point p) = p^.v_normal
calcNormal (Triangle v1 v2 _) = (v1^.v_normal) `cross` (v2^.v_normal)

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

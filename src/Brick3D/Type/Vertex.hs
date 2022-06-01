{-# LANGUAGE TemplateHaskell #-}
module Brick3D.Type.Vertex where

import Brick3D.Type.Base
import Lens.Micro.Platform (makeLenses, (^.))
import Linear.V3 (cross, _x, _y, _z)

-- | One 'Vertex' in 3D coordinate
data Vertex = Vertex { _v_position :: Position-- We might add more information as Vertex attribute
                     } deriving (Show, Eq, Ord)
makeLenses ''Vertex

-- | 'Vertex' that is mapped to Device Coordinate
data DCVertex = DCVertex { _dcv_position :: DCPosition
                         -- | Stores depth Z value of this primitive
                         , _zBuffer :: Float
                         } deriving (Show, Eq, Ord)
makeLenses ''DCVertex

-- | A vertex on Screen Coordinate.
data SCVertex = SCVertex { _scv_position :: SCPosition
                         , _depth :: Float
                         } deriving (Show, Eq, Ord)
makeLenses ''SCVertex

-- | Convert 'Vertex' to 'DCVertex'
fromVertex :: Vertex -> DCVertex
fromVertex v | v^.v_position._z == 0 = DCVertex dcposition 0
             | otherwise            = DCVertex dcposition (abs $ 1 / v^.v_position._z)
             where
               dcposition = V2 (v^.v_position._x) (v^.v_position._y)

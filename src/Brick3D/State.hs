{-# LANGUAGE TemplateHaskell #-}
module Brick3D.State where
import Brick3D.Type
import Brick3D.Camera
import Lens.Micro.Platform
import Tart.Canvas (Canvas)
import Data.Vector (Vector)

-- | Represents one 3D environment.
data ThreeDState = ThreeDState
                   { _camera :: Camera -- ^ Only one camera is supported for now
                   -- | Screen that will be used for rendering.
                   -- We will retrive Aspect ratio from this 'Canvas'
                   , _screen :: Canvas
                   -- | All Primitives that is in the world.
                   , _prims :: Vector Primitive
                   }
makeLenses ''ThreeDState

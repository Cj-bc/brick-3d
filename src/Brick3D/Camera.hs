{-# LANGUAGE TemplateHaskell #-}
module Brick3D.Camera where
import Lens.Micro.Platform
import Linear.V3 
import Linear.Matrix
import Data.Default (Default(..))
import Brick3D.Type

-- | Represents 'Camera' that will be used for rendering
data Camera = Camera { _position :: Position
                     , _rotation :: Rotation
                     , _hFov :: Float
                     , _nearClip :: Float
                     , _farClip :: Float
                     }
makeLenses ''Camera

instance Default Camera where
  def = Camera (V3 0 0 0) identity 60 1 10

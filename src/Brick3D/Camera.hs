{-# LANGUAGE TemplateHaskell #-}
module Brick3D.Camera where
import Lens.Micro.Platform
import Brick3D.Type

-- | Represents 'Camera' that will be used for rendering
data Camera = Camera { _position :: Position
                     , _rotation :: Rotation
                     , _hFov :: Float
                     , _nearClip :: Float
                     , _farClip :: Float
                     }
makeLenses ''Camera

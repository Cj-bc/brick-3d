module Brick3D.Camera where

-- | Represents 'Camera' that will be used for rendering
data Camera = Camera { _position :: Position
                     , _rotation :: Rotation
                     , _hFov :: Float
                     , _nearClip :: Float
                     , _farClip :: Float
                     }
makeLenses ''Camera

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

-- | Move camera's transform
moveCamera :: Camera -> Position -> Camera
moveCamera c diff = c&position+~diff

-- | Rotate Camera
--
-- カメラ自身の場所で回すにはどうすれば？？
rotateCamera :: Camera -> V3 Float -> Camera
rotateCamera c diff = c&rotation%~(\r -> rotationMat !*! r)
  where
    -- 回転行列
    rotation_x theta = V3 (V3              1               0           0)
                          (V3              0      (cos theta) (sin theta))
                          (V3              0  (- (sin theta)) (cos theta))
    rotation_y theta = V3 (V3     (cos theta)              0  (sin theta))
                          (V3              0               1           0)
                          (V3 (- (sin theta))              0  (cos theta))
    -- (cos θ) (-sin θ) 0
    -- (sin θ)  (cos θ) θ
    --      0        0  1
    rotation_z theta = V3 (V3     (cos theta) (- (sin theta))          0)
                          (V3     (sin theta)     (cos theta)          0)
                          (V3              0               0           1)
    -- | Combine all rotation matrix into one
    rotationMat = rotation_x (diff^._x) !*! rotation_y (diff^._y) !*! rotation_z (diff^._z)
                        

module Brick3D.Renderer where
import Brick3D.State
import Brick3D.Camera
import Brick3D.Type
import Brick3D.Rasterization

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Graphics.Vty.Attributes (Attr)
import Lens.Micro.Platform
import Linear.V3 (V3(..), _x, _y, _z, _xyz)
import Linear.V4 (V4(..))
import Linear.Matrix (mkTransformationMat, (!*), identity)
import Tart.Canvas

-- | Renders 'ThreeDState' to one 'Tart.Canvas.Canvas',
-- which will be shown in 'Widget'
render  :: MonadIO m => ThreeDState -> m ThreeDState
render s = do
  let rasteriezd = render' s
  screen' <- liftIO $ clearCanvas (s^.screen) >>= flip canvasSetMany rasteriezd
  pure $ s&screen.~screen'

-- | Do rendering process other than IO operation (which is done in 'render'
render' :: ThreeDState -> [((Int, Int), Char, Attr)]
render' s =
  -- Convert to viewport coordinate
  let cam = s^.camera
      focalLength = abs $ 1/(tan $ (pi/180)*(cam^.hFov)/2)
      -- Apply camera transform
      prims' = applyCameraTransform cam <$> s^.prims
      -- Convert to device coordinate
      -- Also, calculate Normal for later use(e.g. shading)
      dcprims :: Vector DCPrimitive
      dcprims = V.catMaybes $ projectPrimitive focalLength <$> V.filter (farNearClip cam) prims'
      screen' = s^.screen
  -- Geometry Construction
  -- Shading by using property
  -- Rasterize
  in  rasterizeMany (canvasSize screen') dcprims

-- | 'True' if given 'Primitive' is not clipped
-- by far/near plane
-- 
-- far/near平面によって描画されているかを確認する。
-- x,y方向の確認はしない
farNearClip :: Camera -> Primitive -> Bool
farNearClip cam target = let camZ = cam^.position._z :: Float
                             far = cam^.farClip
                             near = cam^.nearClip
                         in all (farNearClipVertex far near camZ) (target^..vertices)
  where
    farNearClipVertex :: Float -> Float -> Float -> Vertex -> Bool
    farNearClipVertex far near camZ v = let tZ   = v^.v_position._z
                                        -- 画面手前方向にz軸は向かっているので, 奥側に伸ばしたい際は
                                        -- 引く。
                                        in camZ-near >= tZ && tZ >= camZ-far


-- | Project one 'Primitive' to device coordinate
projectPrimitive :: Float -> Primitive -> Maybe DCPrimitive
projectPrimitive focalLength prim =
  toDCPrimitive (projectVertex focalLength) prim

-- | Project one vertex to device coordinate
projectVertex :: Float -> Vertex -> Maybe DCVertex
projectVertex focalLength v
  -- focial length <0 means "screen is behind the Camera",
  -- and I don't have any way to render it.
  | focalLength <= 0 = Nothing
  -- Avoid division by zero error
  | v^.v_position._z == 0 = pure $ fromVertex v
  | otherwise =
    let camera2screenVector = -focalLength
        percentage = camera2screenVector/(v^.(v_position._z))
    in pure . fromVertex $ v&v_position._x%~(fixMinusZero . (* percentage))&v_position._y%~(fixMinusZero . (* percentage))
  where
    -- | Convert -0.0 to 0
    -- It's same in most cases,
    -- but sometimes causes problem (e.g. hspec test).
    -- So I replace it with 0.0, which means the same value
    fixMinusZero n | n == -0.0 = 0
                   | otherwise = n


applyCameraTransform :: Camera -> Primitive -> Primitive
applyCameraTransform cam = over (vertices.v_position) (\n -> (transformMatrix !* (conv324 n))^._xyz)
  where
    transformMatrix = mkTransformationMat (cam^.rotation) (- cam^.position) 
    conv324 (V3 x y z) = V4 x y z 1

  

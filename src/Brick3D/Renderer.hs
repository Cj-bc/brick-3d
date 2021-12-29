module Brick3D.Renderer where
import Brick3D.State
import Brick3D.Camera
import Brick3D.Type

import Data.Vector (Vector)
import qualified Data.Vector as V
import Graphics.Vty.Attributes (Attr, defAttr)
import Lens.Micro.Platform
import Linear.V3 (_x, _y, _z)
import Tart.Canvas
import Data.Foldable (fold)

-- | Renders 'ThreeDState' to one 'Tart.Canvas.Canvas',
-- which will be shown in 'Widget'
render  :: MonadIO m => ThreeDState -> m ThreeDState
render s = do
  let rasteriezd = render' s
  screen' <- liftIO $ canvasSetMany (s^.screen) rasteriezd
  pure $ s&screen.~screen'

-- | Do rendering process other than IO operation (which is done in 'render'
render' :: ThreeDState -> [((Int, Int), Char, Attr)]
render' s =
  -- Convert to viewport coordinate
  let cam = s^.camera
      focalLength = 1/(tan $ (cam^.hFov)/2)
      -- Convert to device coordinate
      -- Also, calculate Normal for later use(e.g. shading)
      dcprims :: Vector DCPrimitive
      dcprims = projectPrimitive focalLength <$> V.filter (farNearClip cam) (s^.prims)
      screen' = s^.screen
  -- Geometry Construction
  -- Shading by using property
  -- Rasterize
  in fold $ fmap (rasterize (canvasSize screen')) dcprims

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
    farNearClipVertex far near camZ v = let tZ   = v^.v_normal._z
                                        -- 画面手前方向にz軸は向かっているので, 奥側に伸ばしたい際は
                                        -- 引く。
                                        in camZ-near >= tZ && tZ >= camZ-far


-- | Project one 'Primitive' to device coordinate
projectPrimitive :: Float -> Primitive -> DCPrimitive
projectPrimitive focalLength prim =
  let prim' = over (vertices) (projectVertex focalLength) prim
      norm = calcNormal prim
  in DCPrimitive prim' norm

-- | Project one vertex to device coordinate
projectVertex :: Float -> Vertex -> Vertex
projectVertex focalLength v = 
  let percentage = focalLength/(v^.(v_normal._z))
  in v&v_normal%~(fmap (* percentage))


-- | Rasterize one 'DCPrimitive'
rasterize :: (Int, Int) -> DCPrimitive -> [((Int, Int), Char, Attr)]
rasterize (sx, sy) (DCPrimitive shape normal) =
  case shape of
    Point v ->
      [(rasterizeVertex v
       , '*'
       , defAttr
       )]
    tri@(Triangle v1 v2 v3) ->
      flip fmap (tri^..vertices) $ \v ->
                                     (rasterizeVertex v
                                     , 't'
                                     , defAttr
                                     )
  where
    halfX = round $ (fromRational.toRational $ sx :: Float)/2
    halfY = round $ (fromRational.toRational $ sy :: Float)/2
    moveOriginToCenter (x, y) =  (x+halfX, y+halfY)
    rasterizeVertex v = moveOriginToCenter (round $ (fromInteger . toInteger $ sx) * v^.v_normal._x
                                           , round $ (fromInteger . toInteger $ sy) * v^.v_normal._y)

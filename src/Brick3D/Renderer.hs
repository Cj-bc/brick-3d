module Brick3D.Renderer where
import Brick3D.State
import Brick3D.Camera
import Brick3D.Type

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Graphics.Vty.Attributes (Attr, defAttr)
import Lens.Micro.Platform
import Linear.V3 (V3(..), _x, _y, _z, _xyz)
import Linear.V4 (V4(..))
import Linear.Matrix (mkTransformationMat, (!*), identity)
import Tart.Canvas
import Data.Foldable (fold)
import Linear.Vector ((^*), (^+^))

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
      focalLength = abs $ 1/(tan $ (cam^.hFov)/2)
      -- Apply camera transform
      prims' = applyCameraTransform cam <$> s^.prims
      -- Convert to device coordinate
      -- Also, calculate Normal for later use(e.g. shading)
      dcprims :: Vector DCPrimitive
      dcprims = projectPrimitive focalLength <$> V.filter (farNearClip cam) prims'
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
    farNearClipVertex far near camZ v = let tZ   = v^.v_position._z
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
  let camera2screenVector = -focalLength
      percentage = camera2screenVector/(v^.(v_position._z))
  in v&v_position%~(fmap (* percentage))


applyCameraTransform :: Camera -> Primitive -> Primitive
applyCameraTransform cam = over (vertices.v_position) (\n -> (transformMatrix !* (conv324 n))^._xyz)
  where
    transformMatrix = mkTransformationMat (cam^.rotation) (- cam^.position) 
    conv324 (V3 x y z) = V4 x y z 1
-- applyCameraTransform :: Camera -> Primitive -> (Camera, Primitive)
-- applyCameraTransform cam prim = ((cam&position.~(V3 0 0 0)), (prim&vertices.position%~(\n -> n - cam^.position)))
  

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
      let outlineVertices = rasterizeLine v1 v3 <> rasterizeLine v1 v2 <> rasterizeLine v2 v3
      in flip fmap outlineVertices $ \v ->
                                       (rasterizeVertex v
                                       , '.'
                                       , defAttr
                                       )
  where
    halfX = round $ (fromRational.toRational $ sx :: Float)/2
    halfY = round $ (fromRational.toRational $ sy :: Float)/2
    moveOriginToCenter (x, y) =  (x+halfX, y+halfY)
    rasterizeVertex v = moveOriginToCenter ( round $ (fromInteger . toInteger $ sx) * v^.v_position._x
                                           , - (round $ (fromInteger . toInteger $ sy) * v^.v_position._y)
                                           )

-- | 'Vertex's which constructs line begin at 'begin' and end at 'end'
--
-- JP: 与えられた 'begin' と 'end' を両端に持つ線分を構成する 'Vertex' を返します
rasterizeLine :: Vertex -> Vertex -> [Vertex]
rasterizeLine begin end = let v = end^.v_position - begin^.v_position :: Normal
                              formula t = (begin^.v_position) ^+^ (v ^* t)
                              ts = fmap (/ 500) [0..500] :: [Float]
                          in fmap (\t -> begin&v_position.~(formula t)) ts

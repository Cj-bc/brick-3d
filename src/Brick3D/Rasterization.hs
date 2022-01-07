module Brick3D.Rasterization where
import Brick3D.Type

import Data.Map (Map)
import qualified Data.Map as M
import Graphics.Vty.Attributes (Attr, defAttr)
import Lens.Micro.Platform
import Linear.V2 (V2(..))
import Linear.V3 (_x, _y, _z)
import Linear.Vector ((^*), (^+^))

  -- | Represents one Pixel
type PixelAttr = (Char, Attr)

-- | Rasterize one 'DCPrimitive'
rasterize :: (Int, Int) -> DCPrimitive -> Map (Int, Int) (Float, PixelAttr)
rasterize (sx, sy) (DCPrimitive shape normal) =
  case shape of
    Point v ->
      M.singleton (rasterizeVertex v) (v^.zBuffer, ('*', defAttr))
    tri@(Triangle v1 v2 v3) ->
      let outlineVertices = rasterizeLine v1 v3 <> rasterizeLine v1 v2 <> rasterizeLine v2 v3
      in M.fromList . flip fmap outlineVertices $ \v ->
                                                    ((rasterizeVertex v)
                                                    , (v^.zBuffer
                                                      , ('*', defAttr)))
  where
    halfX = round $ (fromRational.toRational $ sx :: Float)/2
    halfY = round $ (fromRational.toRational $ sy :: Float)/2
    moveOriginToCenter (x, y) =  (x+halfX, y+halfY)
    rasterizeVertex v = moveOriginToCenter ( round $ (fromInteger . toInteger $ sx) * v^.dcv_position._x
                                           , - (round $ (fromInteger . toInteger $ sy) * v^.dcv_position._y)
                                           )

-- | 'Vertex's which constructs line begin at 'begin' and end at 'end'
--
-- JP: 与えられた 'begin' と 'end' を両端に持つ線分を構成する 'Vertex' を返します
rasterizeLine :: DCVertex -> DCVertex -> [DCVertex]
rasterizeLine begin end = let v = end^.dcv_position - begin^.dcv_position :: V2 Float
                              formula t = (begin^.dcv_position) ^+^ (v ^* t)
                              ts = fmap (/ 500) [0..500] :: [Float]
                          in fmap (\t -> begin&dcv_position.~(formula t)) ts
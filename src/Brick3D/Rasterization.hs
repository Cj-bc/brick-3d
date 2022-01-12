module Brick3D.Rasterization where
import Brick3D.Type

import Data.Map (Map)
import Data.Bool (bool)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Map as M
import Graphics.Vty.Attributes (Attr, defAttr)
import Lens.Micro.Platform
import Linear.V2 (V2(..))
import Linear.V3 (_x, _y, _z)
import Linear.Vector ((^*), (^+^))
import Linear.Metric (dot, distance)

  -- | Represents one Pixel
type PixelAttr = (Char, Attr)

-- | Merge two 'Map' of Pixels into one by comparing zBuffer
mergeAttr :: Map (Int, Int) (Float, PixelAttr) -> Map (Int, Int) (Float, PixelAttr) -> Map (Int, Int) (Float, PixelAttr)
mergeAttr m1 m2 = (M.intersectionWith (\a1 a2 -> bool a2 a1 (a1^._1 >= a2^._1)) m1 m2)
                  <> m1 <> m2

-- | Convert 'Map' to list so that 'canvasSetMany' can treat
toCanvasPixels :: Map (Int, Int) (Char, Attr) -> [((Int, Int), Char, Attr)]
toCanvasPixels = M.foldlWithKey (\acc k v -> (k, v^._1, v^._2) : acc ) []

-- | Rasterize many of 'DCPrimitive's
rasterizeMany :: (Foldable t, Functor t) => (Int, Int) -> t DCPrimitive -> [((Int, Int), Char, Attr)]
rasterizeMany screenSize prims =
  toCanvasPixels . fmap (^._2) . foldr mergeAttr mempty $ fmap (rasterize screenSize) prims

-- | Rasterize one 'DCPrimitive'
rasterize :: (Int, Int) -> DCPrimitive -> Map (Int, Int) (Float, PixelAttr)
rasterize (sx, sy) (DCPrimitive shape normal) =
  case shape of
    Point v ->
      M.singleton (rasterizeVertex v) (v^.zBuffer, ('*', defAttr))
    tri@(Triangle v1 v2 v3) ->
      let outlineVertices = rasterizeLine v1 v3 <> rasterizeLine v1 v2 <> rasterizeLine v2 v3
      in M.fromList . V.toList . flip fmap outlineVertices $ \v ->
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
rasterizeLine :: DCVertex -> DCVertex -> Vector DCVertex
rasterizeLine begin end = let v = end^.dcv_position - begin^.dcv_position :: V2 Float
                              formula t = (begin^.dcv_position) ^+^ (v ^* t)
                              ts = fmap (/ 500) (V.fromList [0..500]) :: Vector Float
                          in fmap (\t -> begin&dcv_position.~(formula t)) ts

-- | Returns 'DCVertex's that constructs one filled-triangle
--
-- Note that depth of those 'DCVertex's are not calculated properly.
-- Currently it inherits depth of 'v1'
-- I want to fix this later.
fillTriangle :: DCVertex -> DCVertex -> DCVertex -> Vector DCVertex
fillTriangle v1 v2 v3 = flip (dcv_position.~) v1
                        <$> V.filter (`isInsideOfTri` (v1^.dcv_position, v2^.dcv_position, v3^.dcv_position))
                        boundaryRectVertices
  where
    maxX = foldr1 max $ (^.dcv_position._x) <$> [v1, v2, v3]
    minX = foldr1 min $ (^.dcv_position._x) <$> [v1, v2, v3]
    maxY = foldr1 max $ (^.dcv_position._y) <$> [v1, v2, v3]
    minY = foldr1 min $ (^.dcv_position._y) <$> [v1, v2, v3]
    boundaryRectVertices = V.fromList [V2 (x/100) (y/100) | x <- [(minX*100)..(maxX*100)]
                                                          , y <- [(minY*100)..(maxY*100)]]

-- | 'True' if given coordinate is within given Triangle
--
-- It doesn't care if the triangle is formed well.
-- (e.g. one vertex is on the line made of other two vertex, two of them are the same.)
-- This is because projection could
--
-- JP: この関数は「三角形がきちんと三角形であるか」を考慮しません。
-- (例えば: 頂点が一直線上に並んでしまっている, 複数の頂点が同じ位置にあるなど)
isInsideOfTri :: V2 Float -> (V2 Float, V2 Float, V2 Float) -> Bool
isInsideOfTri candidate (v1, v2, v3)
  = all (oneLineTest candidate)  [(v1, v2, v3), (v2, v3, v1), (v3, v1, v2)]
  where
    -- | Test wether 'cand' and 'a' is at the same side of line made of vs and ve
    --
    -- JP: 'cand' と 'a' が, vs,ve を両端に持つ直線の同じ側にあるかを判定する。
    -- 同じ側にあれば, その直線と 'cand' 及び 'a' の内積の符号が等しくなる=0以上になる
    -- はずなので判定ができる。
    oneLineTest :: V2 Float -> (V2 Float, V2 Float, V2 Float) -> Bool
    oneLineTest cand (vs, ve, a) = (vl `dot` cand)*(vl `dot` a) >= 0 -- 線上にある場合も含めている
      where
        -- | JP: 確かめたい対象の直線ベクトル
        line = ve - vs
        -- | JP: 対象の線分に直交する直線ベクトル
        -- 直交する場合内積が0になる性質を利用して定義する
        vl = V2 (line^._y) (-line^._x)

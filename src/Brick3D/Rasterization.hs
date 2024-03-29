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
mergeAttr m1 m2 = M.intersectionWith (\a1 a2 -> bool a2 a1 (a1^._1 >= a2^._1)) m1 m2
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
      M.singleton (toTuple $ screenMapping v) (v^.zBuffer, ('*', defAttr))
    tri@(Triangle v1 v2 v3) ->
      let v1' = screenMapping v1
          v2' = screenMapping v2
          v3' = screenMapping v3
          wireframeVertices = rasterizeLine v1' v3' <> rasterizeLine v1' v2' <> rasterizeLine v2' v3'
          fill = fillTriangle v1' v2' v3'
          toOutput :: Char -> Vector SCVertex -> Vector ((Int,Int), (Float, PixelAttr))
          toOutput c = fmap $ \v -> (toTuple v, (v^.depth, (c, defAttr)))
          fillOutput = toOutput 'B' fill
          wireframeOutput = toOutput '*' wireframeVertices
      in M.fromList . V.toList $ fillOutput <> wireframeOutput
  where
    halfX = fromInteger . toInteger $ sx `div` 2 :: Int
    halfY = fromInteger . toInteger $ sy `div` 2 :: Int

    toTuple :: SCVertex -> (Int, Int)
    toTuple v = (v^.scv_position._x, v^.scv_position._y)

    -- Be careful: This is __Not `DCPosition -> SCPosition'__
    --   Because 'halfX' and 'halfY' are both 'SCPosition' value.
    moveOriginToCenter :: SCPosition -> SCPosition
    moveOriginToCenter (V2 x y) =  V2 (x+halfX) (y+halfY)

    screenMapping :: DCVertex -> SCVertex
    screenMapping v = let mappedPos = screenMapping' $ v^.dcv_position
                      in SCVertex mappedPos (v^.zBuffer)

    screenMapping' :: DCPosition -> SCPosition
    screenMapping' v = moveOriginToCenter
                         $ V2 (round $ (fromInteger . toInteger $ sx) * v^._x)
                         (round $ -((fromInteger . toInteger $ sy) * v^._y))

-- | 'DCVertex's which constructs line begin at 'begin' and end at 'end'
--
-- JP: 与えられた 'begin' と 'end' を両端に持つ線分を構成する 'DCVertex' を返します
rasterizeLine :: SCVertex -> SCVertex -> Vector SCVertex
rasterizeLine begin end = let begin' = begin^.scv_position
                              end' = end^.scv_position
                              v = end'-begin'
                              y :: Int -> Int
                              y x = round $ ((fromRational . toRational $ v^._y)
                                             /(fromRational . toRational $ v^._x :: Float))
                                    *fromIntegral x
                          in (\x -> begin&scv_position%~(+ V2 x (y x))) <$> V.generate (end'^._x-begin'^._x) (fromInteger.toInteger)

-- | Returns 'DCVertex's that constructs one filled-triangle
--
-- Note that depth of those 'DCVertex's are not calculated properly.
-- Currently it inherits depth of 'v1'
-- I want to fix this later.
fillTriangle :: SCVertex -> SCVertex -> SCVertex -> Vector SCVertex
fillTriangle v1 v2 v3 = flip (scv_position.~) v1
                        <$> V.filter (`isInsideOfTri` (v1^.scv_position, v2^.scv_position, v3^.scv_position))
                        boundaryRectVertices
  where
    maxX = maximum $ (^.scv_position._x) <$> [v1, v2, v3]
    minX = minimum $ (^.scv_position._x) <$> [v1, v2, v3]
    maxY = maximum $ (^.scv_position._y) <$> [v1, v2, v3]
    minY = minimum $ (^.scv_position._y) <$> [v1, v2, v3]
    boundaryRectVertices = V.fromList [V2 x y | x <- [minX..maxX]
                                              , y <- [minY..maxY]]

-- | 'True' if given coordinate is within given Triangle
--
-- It doesn't care if the triangle is formed well.
-- (e.g. one vertex is on the line made of other two vertex, two of them are the same.)
-- This is because projection could
--
-- JP: この関数は「三角形がきちんと三角形であるか」を考慮しません。
-- (例えば: 頂点が一直線上に並んでしまっている, 複数の頂点が同じ位置にあるなど)
isInsideOfTri :: SCPosition -> (SCPosition, SCPosition, SCPosition) -> Bool
isInsideOfTri candidate (v1, v2, v3)
  = all (oneLineTest candidate)  [(v1, v2, v3), (v2, v3, v1), (v3, v1, v2)]
  where
    -- | Test wether 'cand' and 'a' is at the same side of line made of vs and ve
    --
    -- JP: 'cand' と 'a' が, vs,ve を両端に持つ直線の同じ側にあるかを判定する。
    -- 同じ側にあれば, その直線と 'cand' 及び 'a' の内積の符号が等しくなる=0以上になる
    -- はずなので判定ができる。
    --
    -- TODO: Float判定をした方が精度が良い？
    oneLineTest :: V2 Int -> (V2 Int, V2 Int, V2 Int) -> Bool
    oneLineTest cand (vs, ve, a) = (vl `dot` (cand - vs))*(vl `dot` (a - vs)) >= 0 -- 線上にある場合も含めている
      where
        -- | JP: 確かめたい対象の直線ベクトル
        line = ve - vs
        -- | JP: 対象の線分に直交する直線ベクトル
        -- 直交する場合内積が0になる性質を利用して定義する
        vl = V2 (line^._y) (-line^._x)

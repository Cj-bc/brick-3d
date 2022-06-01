module Brick3D.Type.Base where

import Linear.V3 (V3(..))
import Linear.V2 (V2(..))
import Linear.Matrix (M33(..))

-- | Position in 3-Dimension
type Position = V3 Float
-- | Rotation in 3-Dimension
type Rotation = M33 Float
-- | Position in Device Coordinate
--
-- Device coordnate is a Coordinate that
-- put (0,0) on the center, and Y axis
-- is positive above the center. X axis
-- is positive on the right.
-- The value have range of -1~1
-- and floating number(As shown in the Type
-- definition)
--
-- This is the same as what OpenGL explains
-- <<https://open.gl/drawing#Vertex input>>
-- 
-- デバイス座標系は,画面の中心を原点とする
-- 座標系で, 原点から上に向かってY軸が伸び,
-- 右に向かってX軸が伸びています。
-- 値は-1~1の間の小数を取ります。
type DCPosition = V2 Float
-- | Position in Screen Coordinate
--
-- Screen Coordinate is a Coordinate that
-- is associated with actuall "pixel" of
-- the screen.
-- In this case, as we use 'Tart.Canvas.Canvas'
-- for screen, (0, 0) is set at the Top-Left
-- corner.
type SCPosition = V2 Int
-- | Normal of some 3-Dimensional object
type Normal   = V3 Float

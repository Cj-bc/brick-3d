{-# LANGUAGE TemplateHaskell,OverloadedLists #-}
module Ascii3d.Pipeline.VertexShader where

-- | Input for Vertex Shader
class VertexShaderInput input

class VertexShaderOutput output where
  vo_position :: Getter output (Vector (V2 Float))


class (VertexShaderInput input
      ,VertexShaderOutput output
      ) => VertexShader shader input output where
  vertexShader :: input -> output

data ShapePrimitive = Triangle
                    | Quad
                    | Line


-- runShaders :: (VertexShaderInput input) => (input -> Canvas) -> input -> IO Canvas
-- runShaders input pipeline = 


data VSInput = VSInput { _vertexBuffer :: Vector VertexData
                       , _cameraPos :: Position
                       , _aspectRatio :: Float
                       , _hFov :: Float
                       , screen :: Canvas
                       }
makeLenses ''VSInput
instance VertexShaderInput VSInput

data VSOutput = VSOutput { _deviceCoord :: Vector (V2 Float)
                         , _zBuffer :: Vector Float
                         , _screen :: Canvas
                         }
makeLenses ''VSOutput

instance VertexShaderOutput VSOutput where
  vo_position = deviceCoord

vertexShader :: VSInput -> VSOutput
vertexShader input =
  let 
      faciaLength = 1/(tan (2/input^.hFov))
      positions = fmap (putVertexToDeviceCoord facialLength) $ input^.vertexBuffer
      zB = fmap (flip (/) 1 . get _z)
  in (VSOutput positions zB $ input^.screen)
  where
    -- 一つの頂点を射影平面上に投影する
    putVertexToDeviceCoord :: Float -> VertexData -> V2 Float
    putVertexToDeviceCoord facaiLength v = let ratio = (-facialLength)/(v^._z)
                              in V2 (v^._x*ratio) (v^._y*ratio)
    
-- shapeAssembly :: VSOutput -> [Primitive] -> 
-- shapeAssembly input primitives =
--   where
--     shapeAssembly' :: Vector (V2 Float) -> [Primitive] -> 
--     shapeAssembly' vertices [Line:rest] | length vertices >= 

module Brick3D.Renderer where
import Brick3D.Camera

-- | Renders 'ThreeDState' to one 'Tart.Canvas.Canvas',
-- which will be shown in 'Widget'
render  :: MonadIO m => ThreeDState -> m ThreeDState
render = do
  -- Convert to viewport coordinate
  let focalLength = 
  -- Convert to device coordinate

  -- Geometry Construction
    -- Also, calculate Normal for later use(e.g. shading)
  -- Shading by using property
  -- Rasterize

convertToDeviceCoordinate :: Float -> Vertex -> V2 Float

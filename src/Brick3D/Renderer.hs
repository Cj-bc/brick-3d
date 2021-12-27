module Brick3D.Renderer where
import Brick3D.Camera

-- | Renders 'ThreeDState' to one 'Tart.Canvas.Canvas',
-- which will be shown in 'Widget'
render  :: MonadIO m => ThreeDState -> m ThreeDState
render s = do
  let rasteriezd = render' s
  screen' <- liftIO . canvasSetMany rasterized $ s^.screen
  pure $ s&screen.~screen'

-- | Do rendering process other than IO operation (which is done in 'render'
render' :: ThreeDState -> [((Int, Int), Char, Attr)]
render' s =
  -- Convert to viewport coordinate
  let cam = s^.cam
      focalLength = 1/(tan $ (cam^.hFov)/2)
      -- Convert to device coordinate
  -- Geometry Construction
    -- Also, calculate Normal for later use(e.g. shading)
  -- Shading by using property
  -- Rasterize

convertToDeviceCoordinate :: Float -> Vertex -> V2 Float

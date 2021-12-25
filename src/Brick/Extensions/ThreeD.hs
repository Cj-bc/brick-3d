module Brick.Extensions.ThreeD where
import Tart.Canvas (Canvas)
import Brick

-- | Core widget that displays 3D viewport.
--
-- You *should* call 'render' function in 'EventM'
-- to render image, because it requires IO action.
threeD :: ThreeDState -> Widget n
threeD = raw . canvasLayersToImage . pure . view screen


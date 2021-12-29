module Brick.Extensions.ThreeD.Widgets where
import Lens.Micro.Platform (view)
import Tart.Canvas (Canvas, canvasLayersToImage)
import Brick3D.State
import Brick

-- | Core widget that displays 3D viewport.
--
-- You *should* call 'render' function in 'EventM'
-- to render image, because it requires IO action.
threeD :: ThreeDState -> Widget n
threeD = raw . canvasLayersToImage . pure . view screen

module Brick.Extensions.ThreeD.Widgets where
import Lens.Micro.Platform (view, (-~), (+~))
import Tart.Canvas (Canvas, canvasLayersToImage)
import Brick3D.Camera
import Brick3D.Renderer
import Brick3D.State
import Brick hiding (render)
import Graphics.Vty
import Linear.V3 (_x, _y, _z)
import Control.Monad.IO.Class (liftIO)

-- | Core widget that displays 3D viewport.
--
-- You *should* call 'render' function in 'EventM'
-- to render image, because it requires IO action.
threeD :: ThreeDState -> Widget n
threeD = raw . canvasLayersToImage . pure . view screen


handle3DEvent :: Event -> ThreeDState -> EventM n ThreeDState
handle3DEvent e s = liftIO $ render (modifier s)
  where
    modifier :: ThreeDState -> ThreeDState
    modifier = case e of
                 EvKey (KChar 'w') [] -> camera.position._z-~1
                 EvKey (KChar 's') [] -> camera.position._z+~1
                 EvKey (KChar 'a') [] -> camera.position._x-~1
                 EvKey (KChar 'd') [] -> camera.position._x+~1
                 EvKey (KChar 'q') [] -> camera.position._y-~1
                 EvKey (KChar 'e') [] -> camera.position._y+~1
                 _ -> id

module Brick.Extensions.ThreeD.Widgets where
import Lens.Micro.Platform (view, (-~), (+~), (%~))
import Tart.Canvas (Canvas, canvasLayersToImage)
import Brick3D.Camera
import Brick3D.Renderer
import Brick3D.State
import Brick hiding (render)
import Graphics.Vty
import Linear.V3 (V3(..), _x, _y, _z)
import Control.Monad.IO.Class (liftIO)

-- | Core widget that displays 3D viewport.
--
-- You *should* call 'render' function in 'EventM'
-- to render image, because it requires IO action.
threeD :: ThreeDState -> Widget n
threeD = raw . canvasLayersToImage . pure . view screen


handle3DEvent :: Event -> EventM n ThreeDState ()
handle3DEvent e = get >>= liftIO . render . modifier >>= put
  where
    modifier :: ThreeDState -> ThreeDState
    modifier = case e of
                 EvKey (KChar 'w') [] -> camera%~(moveCamera (V3   0    0  (-1)))
                 EvKey (KChar 's') [] -> camera%~(moveCamera (V3   0    0    1))
                 EvKey (KChar 'a') [] -> camera%~(moveCamera (V3 (-1)   0    0))
                 EvKey (KChar 'd') [] -> camera%~(moveCamera (V3   1    0    0))
                 EvKey (KChar 'q') [] -> camera%~(moveCamera (V3   0  (-1)   0))
                 EvKey (KChar 'e') [] -> camera%~(moveCamera (V3   0    1    0))
                 EvKey (KChar 'h') [] -> camera%~(rotateCamera (V3         0    (pi/180)        0))
                 EvKey (KChar 'l') [] -> camera%~(rotateCamera (V3         0  (- pi/180)        0))
                 EvKey (KChar 'k') [] -> camera%~(rotateCamera (V3   (pi/180)         0         0))
                 EvKey (KChar 'j') [] -> camera%~(rotateCamera (V3 (- pi/180)         0         0))
                 EvKey (KChar 'o') [] -> camera%~(rotateCamera (V3         0          0   (pi/180)))
                 EvKey (KChar 'p') [] -> camera%~(rotateCamera (V3         0          0 (- pi/180)))
                 _ -> id

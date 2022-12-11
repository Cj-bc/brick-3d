{-# LANGUAGE OverloadedLists #-}
module Main where
import Data.Vector (Vector(..))
import Linear.V3
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import qualified Graphics.Vty as Vty
import Brick
import Brick.Widgets.Border
import Tart.Canvas
import Brick3D.Type
import Brick3D.State
import Brick3D.Camera
import qualified Brick3D.Renderer as B3DR
import Brick.Extensions.ThreeD.Widgets

data AppEvent = NoEvent
data AppName = NoName deriving (Ord, Eq)

app :: App ThreeDState AppEvent AppName
app = App { appDraw = \s -> [border $ threeD s]
          , appChooseCursor = neverShowCursor
          , appHandleEvent = eHandler
          , appStartEvent = get >>= liftIO . B3DR.render >>= put
          , appAttrMap = const $ attrMap Vty.defAttr []
          }
            
eHandler :: BrickEvent AppName AppEvent -> EventM AppName ThreeDState ()
eHandler (VtyEvent (Vty.EvKey (Vty.KEsc) [])) = halt
eHandler (VtyEvent (Vty.EvKey (Vty.KChar 'r') [])) = get >>= liftIO . B3DR.render >>= put
eHandler (VtyEvent e) = handle3DEvent e
eHandler _ = pure ()

main :: IO ()
main = do
  c <- newCanvas (300, 100)
  let cam          = def { _hFov = 90, _nearClip = 0.1, _farClip = 500 }
      initialState = ThreeDState cam c oneTriangle
  void $ defaultMain app initialState 

-- * Example Set of Primitives

  
cubeLike :: Vector Primitive
cubeLike = [ Triangle (Vertex $ V3 (-30) (-30) (-10)) (Vertex $ V3 (-30) (-30) (-30)) (Vertex $ V3 (-30) 30 (-30))
           , Triangle (Vertex $ V3 (-30) (-30) (-10)) (Vertex $ V3   (-30)    30  (-10)) (Vertex $ V3 (-30) 30 (-30))
           , Triangle (Vertex $ V3 (-30) (-30) (-10)) (Vertex $ V3 (-30) (-30) (-30)) (Vertex $ V3 30 (-30) (-30))
           , Triangle (Vertex $ V3 (-30) (-30) (-10)) (Vertex $ V3 (30) (-30) (-10)) (Vertex $ V3 30 (-30) (-30))
           , Triangle (Vertex $ V3 30 (-30) (-10)) (Vertex $ V3 30 (-30) (-30)) (Vertex $ V3 30 30 (-30))
           , Triangle (Vertex $ V3 30 (-30) (-10)) (Vertex $ V3 30    30  (-10)) (Vertex $ V3 30 30 (-30))
           ]

oneTriangle :: Vector Primitive
oneTriangle = [Triangle (Vertex $ V3 (-10) 10 (-30)) (Vertex $ V3 10 10 (-30)) (Vertex $ V3 10 (-10) (-30))]

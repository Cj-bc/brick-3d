{-# LANGUAGE OverloadedLists #-}
module Main where
import Linear.V3
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
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
          , appStartEvent = \s -> do
              s' <- liftIO $ B3DR.render s
              return s'
          , appAttrMap = const $ attrMap Vty.defAttr []
          }
            
eHandler :: ThreeDState -> BrickEvent AppName AppEvent -> EventM AppName (Next ThreeDState)
eHandler s (VtyEvent (Vty.EvKey (Vty.KEsc) [])) = halt s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'r') [])) = do
  s' <- liftIO $ B3DR.render s
  continue s'
eHandler s (VtyEvent e) = handle3DEvent e s >>= continue
eHandler s _ = continue s

main :: IO ()
main = do
  c <- newCanvas (300, 100)
  let cam          = Camera (V3 0 0 0) (V3 0 0 0) 90 0.1 500
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

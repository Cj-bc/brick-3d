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
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'r') [])) = do
  s' <- liftIO $ B3DR.render s
  continue s'
eHandler s _ = continue s

main :: IO ()
main = do
  c <- newCanvas (50, 30)
  let cam          = Camera (V3 0 0 0) (V3 0 0 0) 60 1 10
      initialState = ThreeDState cam c [Point . Vertex $ V3 0 0 (-3)
                                       , Triangle (Vertex $ V3 2 0 (-3)) (Vertex $ V3 2 2 (-3)) (Vertex $ V3 0 2 (-3))
                                       ]
  void $ defaultMain app initialState 

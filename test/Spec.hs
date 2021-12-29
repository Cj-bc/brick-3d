import Test.Brick3D.Type
import Test.Brick3D.Renderer
import Test.Hspec

main :: IO ()
main = hspec $ do
  verticesTest
  rendererTest
